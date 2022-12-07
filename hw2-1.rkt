#lang plai



(define eight-principles
  (list
   "Know your rights."
   "Acknowledge your sources."
   "Protect your work."
   "Avoid suspicion."
   "Do your own work."
   "Never falsify a record or permit another person to do so."
   "Never fabricate data, citations, or experimental results."
   "Always tell the truth when discussing your work with your instructor."))

(print-only-errors)

(define-type FnWAE
  [num (n number?)]
  [add (lhs FnWAE?)
       (rhs FnWAE?)]
  [sub (lhs FnWAE?)
       (rhs FnWAE?)]
  [with (name symbol?)
        (bound-expr FnWAE?)
        (body-expr FnWAE?)]
  [id (name symbol?)]
  [if0 (o FnWAE?) (t FnWAE?) (th FnWAE?)] 
  [app (fun-name symbol?)
       (args list?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (params list?)
          (body FnWAE?)])

(define-type DefSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (rest DefSub?)])

;; ----------------------------------------------------------------------

;; parse : s-expression -> FnWAE?
(define (parse s-expr)
  (cond [(number? s-expr)
         (num s-expr)]
        [(symbol? s-expr)
         (id s-expr)]
        [(list? s-expr)
         (when (empty? s-expr)
           (error 'parse "the empty list is not a valid FnWAE"))
         (case (first s-expr)
           [(+)
            (check-pieces s-expr 3 "add")
            (add (parse (second s-expr))
                 (parse (third s-expr)))]
           [(-)
            (check-pieces s-expr 3 "sub")
            (sub (parse (second s-expr))
                 (parse (third s-expr)))]
           [(with)
            (check-pieces s-expr 3 "with")
            (check-pieces (second s-expr) 2 "with binding pair")
            (unless (symbol? (first (second s-expr)))
              (error 'parse "expected variable name, got ~a" (first (second s-expr))))
            (with (first (second s-expr))
                  (parse (second (second s-expr)))
                  (parse (third s-expr)))]
           [(if0)
            (check-pieces s-expr 4 "if0")
            (if0(parse (second s-expr))
                (parse (third s-expr))
                (parse (fourth s-expr)))
            ]
            
           [else
            ;(check-pieces s-expr 2 "app")
            (unless (symbol? (first s-expr))
              (error 'parse "expected function name, got ~a" (first s-expr)))
            (app (first s-expr)
                 (map parse (rest s-expr)))])]
        [else (error 'parse "expected FnWAE, got ~a" s-expr)]))

(define (parse-defn deffun)
  [fundef 
   (first (second deffun))
   (if (equal? (length (rest (second deffun))) (length (remove-duplicates (rest (second deffun)))))
       (rest (second deffun))
       (error "bad syntax"))
   (parse (third deffun))]
  )

;FnWAE (listof FunDef) -> Number
(define (interp-expr exp fundefs)
  (interp exp fundefs initial-def-sub))


(define (check-pieces s-expr n-pieces who)
  (unless (= n-pieces (length s-expr))
    (error 'parse "expected ~a, got ~a" who s-expr)))

(test (parse `1) (num 1))
(test (parse `y) (id 'y))
(test (parse `{+ 1 2}) (add (num 1) (num 2)))
(test (parse `{- 1 2}) (sub (num 1) (num 2)))
(test (parse `{with {x 3} {+ x 2}}) (with 'x (num 3) (add (id 'x) (num 2))))
(test (parse `{f 10}) (app 'f (list (num 10)))) ;IS IT FINE THAT I MADE NUM 10 A LIST HERE? IT FIXED THE ERROR
(test/exn (parse `{+ 1 2 3}) "expected add")

;; ----------------------------------------------------------------------

;; interp : FnWAE? (listof FunDef?) DefSub? -> number?
(define (interp an-fnwae fundefs ds)
  (type-case FnWAE an-fnwae
    [num (n) n]
    [add (l r) (+ (interp l fundefs ds) (interp r fundefs ds))]
    [sub (l r) (- (interp l fundefs ds) (interp r fundefs ds))]
    [id (name) (lookup name ds)]
    [with (name named-expr body)
          (interp body
                  fundefs
                  (aSub name
                        (interp named-expr fundefs ds)
                        ds))]
    [if0 (one two three)
         (if (= 0 (interp one fundefs ds))
             (interp two fundefs ds)
             (interp three fundefs ds))]
    [app (fun-name args)
         
         (define the-fundef (lookup-fundef fun-name fundefs))
         
             ; (type-case FunDef the-fundef
             ;[fundef (fun-name param-name body) ;i dont see the point in typecasing?
             (interp (fundef-body the-fundef)
                     fundefs
                     (asubhelp
                      (fundef-params the-fundef)
                      (map (lambda (arg) (interp arg fundefs ds)) args)
                      (mtSub)))
              
         ]))

;; lookup : symbol? DefSub? -> number?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier")]
    [aSub (name2 val rest)
          (if (equal? name name2)
              val
              (lookup name rest))]))


; deffered substitution with list
; asubhelp : list? list? DefSub?-> DefSub?
(require racket/trace)

;(trace )

(define (asubhelp names values ds)
  (if (and (empty? names) (empty? values))
      ds
      (if (or (and (null? values) (not (null? names)))
                 (and (null? names) (not (null? values))))
(error "wrong arity")
      (asubhelp (rest names) (rest values)
                (aSub  (first names) (first values) ds)))))

;(trace asubhelp)
;; lookup-fundef : symbol? (listof FunDef?) -> FunDef?
(define (lookup-fundef fun-name fundefs)
  (cond [(empty? fundefs)
         (error 'interp "undefined function: ~a" fun-name)]
        [(equal? (fundef-fun-name (first fundefs)) fun-name)
         (first fundefs)]
        [else
         (lookup-fundef fun-name (rest fundefs))]))

;mult and neg
(define mult-and-neg-deffuns
  (list `{deffun {neg? x} {if0 x 1 {neghelp x x}}}
        `{deffun {neghelp x y} {if0 x 0 {if0 y 1 {neghelp {+ x 1} {- y 1}}}}}
        `{deffun {abs x} {if0 {neg? x} {- 0 x} x}}
        `{deffun {flip x} {- 0 x}}
        `{deffun {mult x y} {if0 x 0 {if0 y 0 {multhelp x y {abs x} {abs y} {abs x} {abs y} {flip {abs x} } {flip {abs y}}}}}}

        `{deffun {multhelp x y z a b d g h} {if0 {neg? x} {if0 {neg? y}
                                                               {if0 a (- z b) {multhelp x y {+ z b} {- a 1} b d g h}}
                                                               {if0 a (+ g b) {multhelp x y z {- a 1} b d {- g b} h}}}
                                                 {if0 {neg? y}
                                                      {if0 a (+ g b) {multhelp x y z {- a 1} b d {- g b} h}}
                                                      {if0 a (- z b) {multhelp x y {+ z b} {- a 1} b d g h}}}}}


        ))


;; ----------------------------------------------------------------------
;; tests from last time, updated

(define initial-def-sub (mtSub))
;(trace asubhelp)
(test (interp-expr (parse `{neg? -5}) (map parse-defn (list `{deffun {neg? x} {if0 x 1 {neghelp x x}}}
                                                            `{deffun {neghelp x y} {if0 y 0 {if0 x 1 {neghelp {- x 1} {+ y 1}}}}}))) 0)
(test (interp-expr (parse `{neg? 5}) (map parse-defn (list `{deffun {neg? x} {if0 x 1 {neghelp x x}}}
                                                           `{deffun {neghelp x y} {if0 y 0 {if0 x 1 {neghelp {- x 1} {+ y 1}}}}}))) 1) 
(test (interp-expr (parse `{mult -5 -4}) (map parse-defn mult-and-neg-deffuns)) 20)
(test (interp-expr (parse `{mult -5 4}) (map parse-defn mult-and-neg-deffuns)) -20)
(test (interp-expr (parse `{mult 5 -4}) (map parse-defn mult-and-neg-deffuns)) -20)
(test (interp-expr (parse `{mult 5 4}) (map parse-defn mult-and-neg-deffuns)) 20)

(test (interp-expr (parse `{mult -5 -10}) (map parse-defn mult-and-neg-deffuns)) 50)
(test (interp-expr (parse `{mult 10 -10}) (map parse-defn mult-and-neg-deffuns)) -100)
(test (interp-expr (parse `{if0 0 1 2}) '()) 1)
(test (interp-expr (parse `{if0 1 2 3}) '()) 3)
(test (interp (parse `{if0 0 1 2}) '() initial-def-sub) 1)
(test (interp (parse `{if0 1 2 3}) '() initial-def-sub) 3)
;; 5 -> 5
(test (interp (parse `5) '() initial-def-sub)
      5)
;; -5 -> -5
(test (interp (parse `-5) '() initial-def-sub)
      -5)
;; {+ 1 2} -> 3
(test (interp (parse `{+ 1 2}) '() initial-def-sub)
      3)
;; {- 3 4} -> -1
(test (interp (parse `{- 3 4}) '() initial-def-sub)
      -1)
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (parse `{+ {+ 1 2} {- 3 4}}) '() initial-def-sub)
      2)

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {+ x x}})
              '()
              initial-def-sub)
      6)
#|
x
|#
(test/exn (interp (parse `x) '() initial-def-sub)
          "free identifier")
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp (parse `{+ {with {x {+ 1 2}}
                               {+ x x}}
                         {with {x {- 4 3}}
                               {+ x x}}})
              '()
              initial-def-sub)
      8)
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp (parse `{+ {with {x {+ 1 2}}
                               {+ x x}}
                         {with {y {- 4 3}}
                               {+ y y}}})
              '()
              initial-def-sub)
      8)
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {x {- 4 3}}
                                  {+ x x}}})
              '()
              initial-def-sub)
      2)
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {y {- 4 3}}
                                  {+ x x}}})
              '()
              initial-def-sub)
      6)

(test/exn (interp (parse `{f 10})
                  (list)
                  initial-def-sub)
          "undefined function")
(test (interp (parse `{f 10})
              (list (fundef 'f (list 'x)
                            (parse `{- 20 {twice {twice x}}}))
                    (fundef 'twice (list 'y)
                            (parse `{+ y y})))
              initial-def-sub)
      -20)
(test (interp (parse `{f 10})
              (list (fundef 'f (list 'x)
                            (parse `{- 10 {twice {twice x}}}))
                    (fundef 'twice (list 'y)
                            (parse `{+ y y})))
              initial-def-sub)
      -30)


(test/exn (interp (parse `{with {y 2}
                                {f y}})
                  (list (fundef 'f (list 'x)
                                (parse `{+ y x})))
                  initial-def-sub)
          "free identifier")
(test (interp-expr (num 5)  '()) 5)
(test (interp-expr (parse '(neg? 5)) (map parse-defn mult-and-neg-deffuns)) 1)
(test (interp-expr (parse '(neg? 1))(map parse-defn mult-and-neg-deffuns)) 1)
(test (interp-expr (parse '(neg? 0))(map parse-defn mult-and-neg-deffuns)) 1)
(test (interp-expr (parse '(neg? -1))(map parse-defn mult-and-neg-deffuns)) 0)
(test (interp-expr (parse '(neg? -5))(map parse-defn mult-and-neg-deffuns)) 0)
(test (interp-expr (parse '(flip 3))(map parse-defn mult-and-neg-deffuns)) -3)
(test (interp-expr (parse '(flip -7))(map parse-defn mult-and-neg-deffuns)) 7)
(test (interp-expr (parse '(abs 50))(map parse-defn mult-and-neg-deffuns)) 50)
(test (interp-expr (parse '(abs -50))(map parse-defn mult-and-neg-deffuns)) 50) 
(test (interp-expr (parse '(mult 3 3))(map parse-defn mult-and-neg-deffuns)) 9)
(test (interp-expr (parse '(mult -3 -3))(map parse-defn mult-and-neg-deffuns)) 9)
(test (interp-expr (parse '(mult -3 3))(map parse-defn mult-and-neg-deffuns)) -9)
(test (interp-expr (parse '(mult 3 -3))(map parse-defn mult-and-neg-deffuns)) -9)
(test (interp-expr (parse '(mult 0 0))(map parse-defn mult-and-neg-deffuns)) 0)
(test (interp-expr (parse '(mult 9 0))(map parse-defn mult-and-neg-deffuns)) 0)
(test (interp-expr (parse '(mult 0 9))(map parse-defn mult-and-neg-deffuns)) 0)
(test (interp-expr (parse '(mult -9 0))(map parse-defn mult-and-neg-deffuns)) 0)
(test (interp-expr (parse '(mult 0 -9))(map parse-defn mult-and-neg-deffuns)) 0)
(test (interp-expr (parse '(mult 100 100))(map parse-defn mult-and-neg-deffuns)) 10000)
(test (interp-expr (parse '(abs -1))(map parse-defn mult-and-neg-deffuns)) 1)
(test (interp-expr (parse '(abs 1))(map parse-defn mult-and-neg-deffuns)) 1)
(test (interp-expr (parse '(abs 0))(map parse-defn mult-and-neg-deffuns)) 0)
(test (interp-expr (parse '(abs -5))(map parse-defn mult-and-neg-deffuns)) 5)
(test (interp-expr (parse '(abs 5))(map parse-defn mult-and-neg-deffuns)) 5)
 
;more old tests might be some repeats from above but never hurts to be sure

;; 5 -> 5
(test (interp (parse `5) '()initial-def-sub)
      5)
;; {+ 1 2} -> 3
(test (interp (parse `{+ 1 2}) '()initial-def-sub)
      3)
;; {- 3 4} -> -1
(test (interp (parse `{- 3 4}) '()initial-def-sub)
      -1)
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (parse `{+ {+ 1 2} {- 3 4}}) '()initial-def-sub)
      2)

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {+ x x}})
              '() initial-def-sub)
      6)
#|
x
|#
(test/exn (interp (parse `x) '()initial-def-sub)
          "free identifier")
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp (parse `{+ {with {x {+ 1 2}}
                               {+ x x}}
                         {with {x {- 4 3}}
                               {+ x x}}})
              '()initial-def-sub)
      8)
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp (parse `{+ {with {x {+ 1 2}}
                               {+ x x}}
                         {with {y {- 4 3}}
                               {+ y y}}})
              '()initial-def-sub)
      8)
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {x {- 4 3}}
                                  {+ x x}}})
              '()initial-def-sub)
      2)
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {y {- 4 3}}
                                  {+ x x}}})
              '()initial-def-sub)
      6)




(test/exn (interp (parse '{f x})
(list (parse-defn '{deffun {f a b c} c}))initial-def-sub)
"free identifier")



(test (interp (parse '{f 1 2})
              (list (parse-defn '{deffun {f x y} {+ x y}}))initial-def-sub)
      3)


(test (interp (parse '{+ {f} {f}})
              (list (parse-defn '{deffun {f} 5}))initial-def-sub)
      10)

(test/exn (interp (parse '{with {x y} 1})
(list)initial-def-sub)
"free identifier")
(test/exn (interp (parse '{f 1 2})
(list (parse-defn '{deffun {f x x} {+ x x}}))initial-def-sub)
"bad syntax")
(test/exn (interp (parse '{f x})
(list (parse-defn '{deffun {g a b c} c}))initial-def-sub)
"undefined function")
(test/exn (interp (parse '{f 1})
(list (parse-defn '{deffun {f x y} {+ x y}}))initial-def-sub)
"wrong arity")

(test (parse `1)
      (num 1))
(test (parse `x)
      (id 'x))
(test (parse `{+ 1 2})
      (add (num 1) (num 2)))
(test (parse `{- 1 2})
      (sub (num 1) (num 2)))
(test (parse `{+ 1 {+ 2 3}})
      (add (num 1) (add (num 2) (num 3))))
(test (parse `{with {x 3} {+ x 2}})
      (with 'x (num 3) (add (id 'x) (num 2))))
(test/exn (parse `{+ 1 2 3})
          "expected add")

(test (parse `{x {+ 3 10}})
      (app 'x (list (add (num 3) (num 10)))))


;some more organized tests lol
(test (interp (parse '{f 1 2})
(list (parse-defn '{deffun {f x y} {+ x y}}))initial-def-sub)
3)
(test (interp (parse '{+ {f} {f}})
(list (parse-defn '{deffun {f} 5}))initial-def-sub)
10)
(test/exn (parse-defn '{deffun {f x x} x}) "bad syntax")
(test/exn (interp (parse '{with {x y} 1})
(list)initial-def-sub)
"free identifier")
(test/exn (interp (parse '{f 1 2})
(list (parse-defn '{deffun {f x x} {+ x x}}))initial-def-sub)
"bad syntax")
(test/exn (interp (parse '{f x})
(list (parse-defn '{deffun {g a b c} c}))initial-def-sub)
"undefined function")
(test/exn (interp (parse '{f 1})
(list (parse-defn '{deffun {f x y} {+ x y}}))initial-def-sub)
"wrong arity")
(test/exn (interp (parse '{f x})
(list (parse-defn '{deffun {f a b c} c}))initial-def-sub)
"free identifier")