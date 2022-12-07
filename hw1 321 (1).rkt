#lang plai

;(require racket/trace)

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
;(print-only-errors)
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
  [app (fun-name symbol?)
       (args list?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (params list?)
          (body FnWAE?)])
; <FunDef> = {deffun {<id> <id>*} <FnWAE>}
; <FnWAE> = <num>
; | {+ <FnWAE> <FnWAE>}
; | {- <FnWAE> <FnWAE>}
; | {with {<id> <FnWAE>} <FnWAE>}
; | <id>
; | {<id> <FnWAE>*}

(define (parse s-exp)
  (cond [(number? s-exp)
         (num s-exp)]
        [(symbol? s-exp)
         (id s-exp)]
        [(and (list? s-exp) (not (empty? s-exp)))
         (case (first s-exp)
           [(+)
            (check-pieces s-exp 3 "addition")
            (add (parse (second s-exp))
                 (parse (third s-exp)))]
           [(-)
            (check-pieces s-exp 3 "subtraction")
            (sub (parse (second s-exp))
                 (parse (third s-exp)))]
           [(with)
            (check-pieces s-exp 3 "with")
            (check-pieces (second s-exp) 2 "with binding pair")
            (with (first (second s-exp))
                  (parse (second (second s-exp)))
                  (parse (third s-exp)))]
           [else
            ;(check-pieces s-exp (length s-exp) "application")
            (unless (symbol? (first s-exp))
              (error 'parse "expected function name, got ~a" (first s-exp)))
            (app (first s-exp)
                 (map parse (rest s-exp)))])]
        [else
         (error 'parse "expected expression, got: ~a" s-exp)]))


(define (parse-defn deffun)
  (fundef 
  (first (second deffun))
  (if (equal? (length (rest (second deffun))) (length (remove-duplicates (rest (second deffun)))))
      (rest (second deffun))
     (error "bad syntax"))
  (parse (third deffun)))
  )
  



 
(define (check-pieces exp n expected)
  (unless (= (length exp) n)
    (error 'parse "expected ~a, got: ~a" expected exp)))



;; interp : FnWAE? (listof FunDef?) -> number?
(define (interp an-FnWAE fundefs)
  (type-case FnWAE an-FnWAE
    [num (n) n]
    [add (lhs rhs)
         (+ (interp lhs fundefs) (interp rhs fundefs))]
    [sub (lhs rhs)
         (- (interp lhs fundefs) (interp rhs fundefs))]
    [with (name named-expr body)
          (interp (subst body
                         name
                         (interp named-expr fundefs))
                  fundefs)]
    [id (name)
        (error 'interp "free identifier: ~a" name)]
    [app (fun-name args)
         (define the-fundef (lookup-fundef fun-name
                                           fundefs))
                        ;  (if (= (length (fundef-params the-fundef)) (length args))
                   (interp (subshelp (fundef-body the-fundef) (fundef-params the-fundef) 
                                  (map (lambda (arg) (interp arg fundefs)) args))
                           fundefs)
                  ; (error "wrong arity"))
                 ]))

(define (lookup-fundef name fundefs)
  (cond [(empty? fundefs)
         (error 'interp "undefined function: ~a"
                name)]
        [(equal? (fundef-fun-name (first fundefs))
                 name)
         (first fundefs)]
        [else
         (lookup-fundef name (rest fundefs))]))

;; subst : FnWAE? symbol? number? -> FnWAE?
(define (subst a-FnWAE name value)
  (type-case FnWAE a-FnWAE
    [num (n)
         a-FnWAE]
    [add (l r)
         (add (subst l name value)
              (subst r name value))]
    [sub (l r)
         (sub (subst l name value)
              (subst r name value))]
    [with (name2 named-expr body)
          (with name2 (subst named-expr name value)
                (if (equal? name name2)
                    body
                    (subst body name value)))]
    ;;
    [id (name2)
        (if (equal? name name2)
            (num value)
            a-FnWAE)]
;(require racket/trace) (trace interp)
    [app (fun-name args)
         (app fun-name (map (lambda (arg) (subst arg name value)) args))]))

;subhelpfunction

(define (subshelp afnwae names values)
  (if (= (length names) (length values))
      (if (and (empty? names) (empty? values))
          afnwae
          (subshelp (subst afnwae (first names) (first values))
           (rest names)
           (rest values)))
      (error "wrong arity")))
;; 5 -> 5
(test (interp (parse `5) '())
      5)
;; {+ 1 2} -> 3
(test (interp (parse `{+ 1 2}) '())
      3)
;; {- 3 4} -> -1
(test (interp (parse `{- 3 4}) '())
      -1)
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (parse `{+ {+ 1 2} {- 3 4}}) '())
      2)

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {+ x x}})
              '())
      6)
#|
x
|#
(test/exn (interp (parse `x) '())
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
              '())
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
              '())
      8)
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {x {- 4 3}}
                                  {+ x x}}})
              '())
      2)
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (parse `{with {x {+ 1 2}}
                            {with {y {- 4 3}}
                                  {+ x x}}})
              '())
      6)


#|
substitute 10 for x in {+ 1 x}
|#
(test (subst (add (num 1) (id 'x))
             'x
             10)
      (add (num 1) (num 10)))
#|
substitute 10 for x in y
|#
(test (subst (id 'y) 'x 10)
      (id 'y))
#|
substitute 10 for x in {- 1 x}
|#
(test (subst (sub (num 1) (id 'x))
             'x
             10)
      (sub (num 1) (num 10)))
#|
substitute 10 for x in {with {y 17} x}
|#
(test (subst (with 'y (num 17) (id 'x))
             'x
             10)
      (with 'y (num 17) (num 10)))
#|
substitute 10 for x in {with {y x} y}
|#
(test (subst (with 'y (id 'x) (id 'y))
             'x
             10)
      (with 'y (num 10) (id 'y)))
#|
substitute 10 for x in {with {x y} x}
|#
(test (subst (with 'x (id 'y) (id 'x))
             'x
             10)
      (with 'x (id 'y) (id 'x)))


;(trace interp)
;(trace subst)

(test/exn (interp (parse '{f x})
(list (parse-defn '{deffun {f a b c} c})))
"free identifier")



(test (interp (parse '{f 1 2})
              (list (parse-defn '{deffun {f x y} {+ x y}})))
      3)


(test (interp (parse '{+ {f} {f}})
              (list (parse-defn '{deffun {f} 5})))
      10)

(test/exn (interp (parse '{with {x y} 1})
(list))
"free identifier")
(test/exn (interp (parse '{f 1 2})
(list (parse-defn '{deffun {f x x} {+ x x}})))
"bad syntax")
(test/exn (interp (parse '{f x})
(list (parse-defn '{deffun {g a b c} c})))
"undefined function")
(test/exn (interp (parse '{f 1})
(list (parse-defn '{deffun {f x y} {+ x y}})))
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
(list (parse-defn '{deffun {f x y} {+ x y}})))
3)
(test (interp (parse '{+ {f} {f}})
(list (parse-defn '{deffun {f} 5})))
10)
(test/exn (parse-defn '{deffun {f x x} x}) "bad syntax")
(test/exn (interp (parse '{with {x y} 1})
(list))
"free identifier")
(test/exn (interp (parse '{f 1 2})
(list (parse-defn '{deffun {f x x} {+ x x}})))
"bad syntax")
(test/exn (interp (parse '{f x})
(list (parse-defn '{deffun {g a b c} c})))
"undefined function")
(test/exn (interp (parse '{f 1})
(list (parse-defn '{deffun {f x y} {+ x y}})))
"wrong arity")
(test/exn (interp (parse '{f x})
(list (parse-defn '{deffun {f a b c} c})))
"free identifier")