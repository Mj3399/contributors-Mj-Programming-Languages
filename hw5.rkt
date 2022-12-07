#lang plai
(print-only-errors)

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
;<EFAE> = <num>
;| {+ <EFAE> <EFAE>}
;| {- <EFAE> <EFAE>}
;| <id>
;| {fun {<id>} <EFAE>}
;| {<EFAE> <EFAE>} ;; function application
;| {if0 <EFAE> <EFAE> <EFAE>}
;| {throw <id> <EFAE>}
;| {try <EFAE1> {catch {tag <id1> as <id2>} <EFAE2>}}

;
;(define-type KFAE
;  [num (n number?)]
;  [add (lhs KFAE?)
;       (rhs KFAE?)]
;  [sub (lhs KFAE?)
;       (rhs KFAE?)]
;  [id (name symbol?)]
;  [fun (param-name symbol?)
;       (body KFAE?)]
;  [app (fun-expr KFAE?)
;       (arg-expr KFAE?)]
;  [ret-0]
;  [ret (ret-expr KFAE?)])

(define-type EFAE
  [num (n number?)]
  [add (lhs EFAE?)
       (rhs EFAE?)]
  [sub (lhs EFAE?)
       (rhs EFAE?)]
  [id (name symbol?)]
  [fun (param symbol?)
       (body EFAE?)]
  [app (fun-expr EFAE?)
       (arg-expr EFAE?)]
  [if0 (tst EFAE?)
       (thn EFAE?)
       (els EFAE?)]
  [throw (tag symbol?)
         (throw-expr EFAE?)]
  [try-catch (try-body EFAE?)
             (tag symbol?)
             (exn-name symbol?)
             (catch-body EFAE?)])

(define-type EFAE-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body EFAE?)
            (ds DefSub?)])

(define-type DefSub
  [mtSub]
  [aSub  (name symbol?)
         (value EFAE-Value?)
         (rest DefSub?)])

(define-type Cont
  [numop-do-right  (rhs EFAE?)
                   (ds  DefSub?)
                   (op (-> number? number? number?))
                   (rest-k Cont?)]
  [numop-do-op     (l-val EFAE-Value?)
                   (op (-> number? number? number?))
                   (rest-k Cont?)]
  [app-do-arg      (arg-expr EFAE?)
                   (ds  DefSub?)
                   (rest-k Cont?)]
  [app-do-body     (fun-val EFAE-Value?)
                   (rest-k Cont?)]
  [app-do-return   (rest-k Cont?)]
  [do-early-return (t symbol?)
                   (rest-k Cont?)]
  [catch? (tag symbol?)
          (ex symbol?)
          (exp EFAE?)
          (ds DefSub?)
          (rest-k Cont?)]
  [dof

   (two EFAE?)
   (three EFAE?)
   (ds  DefSub?)
   (rest-k Cont?) ]
  [done])

;; ----------------------------------------------------------------------

;; parse : s-expression -> EFAE?
(define (parse s-exp)
  (cond [(number? s-exp)
         (num s-exp)]
        [(symbol? s-exp)
         (id s-exp)]
        [(list? s-exp)
         (when (empty? s-exp)
           (error 'parse "the empty list is not a valid EFAE"))
         (case (first s-exp)
           [(+)
            ;(check-pieces s-exp "add" 3)
            (add (parse (second s-exp))
                 (parse (third s-exp)))]
           [(-)
            ;(check-pieces s-exp "sub" 3)
            (sub (parse (second s-exp))
                 (parse (third s-exp)))]
           [(fun)
            ;(check-pieces s-exp "fun" 3)
            ;(check-pieces (second s-exp) "parameter list" 1)
            (fun (first (second s-exp))
                 (parse (third s-exp)))]
           [(with) ; in lieu of a compiler
            ;(check-pieces s-exp "with" 3)
            ;(check-pieces (second s-exp) "with binding pair" 2)
            (unless (symbol? (first (second s-exp)))
              (error 'parse "expected variable name, got ~a" (first (second s-exp))))
            (app (fun (first (second s-exp)) (parse (third s-exp)))
                 (parse (second (second s-exp))))]
           [(if0)
            ;(check-pieces s-exp 4 "if0")
            (if0 (parse (second s-exp))
                 (parse (third s-exp))
                 (parse (fourth s-exp)))
            ]
           [(throw)
            ;(check-pieces s-exp 3 "throw")
            (throw
             (second s-exp)
             (parse (third s-exp)))]
           [(try)
            ;(check-pieces s-exp 3 "try")
            (try-catch
             (parse (second s-exp))
             ;(check-pieces (third s-exp) 3 "catch")
             ;(check-pieces (second (third s-exp)) 4 "tag")
             (second (second (third s-exp)))
             (fourth (second (third s-exp)))
             (parse (third (third s-exp))))]
           
           [else
            (check-pieces s-exp "app" 2)
            (app (parse (first s-exp))
                 (parse (second s-exp)))])]
        [else
         (error 'parse "expected EFAE got ~a" s-exp)]))

(define (check-pieces s-exp expected n-pieces)
  (unless (and (list? s-exp)
               (= n-pieces (length s-exp)))
    (error 'parse "expected ~a got ~a" expected s-exp)))

;; ----------------------------------------------------------------------

;; interp-test : s-expression -> EFAE-Value?
(define (interp-test s-exp)
  
  (interp (parse s-exp) (mtSub)
          (done)))
(define (unwrap g)
  
  (type-case EFAE-Value g
    [numV (n) n]
    [closureV (param-name body closed-ds)
              'function])
  )
                      
(define (interp-expr a-EFAE)
  
  (unwrap (interp a-EFAE (mtSub)
                  (done)))
          
  )
(define initial-def-sub (mtSub))
(require racket/pretty)
(define (log-interp a-EFAE k)
  ;(printf "interp:\na-EFAE: ~a\nk: ~a\n\n"
  (pretty-format a-EFAE)
  (pretty-format k))

;; interp : EFAE? DefSub? Cont? -> EFAE-Value?
(define (interp a-EFAE ds k)
  (log-interp a-EFAE k)
  (type-case EFAE a-EFAE
    [num (n) (interp-cont (numV n) k)]
    [id (name) (interp-cont (lookup name ds) k)]
    [fun (param-name body) (interp-cont (closureV param-name body ds) k)]
    [add (l r) (numop + l r ds k)]
    [sub (l r) (numop - l r ds k)]
    [if0 (one two three)
         (interp one ds (dof  two three ds k))]
    [app (fun-expr arg-expr)
         (interp fun-expr ds
                 (app-do-arg arg-expr ds
                             k))]
    
    [try-catch (try tag ex catch)
               
               (interp try ds (catch? tag ex catch ds k))]   
     
    [throw (tag exp)
      
           (interp exp ds (do-early-return tag k))]))


;; lookup : symbol? DefSub? -> EFAE-Value?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error "free identifier")]
    [aSub (name2 value rest)
          (if (equal? name name2)
              value
              (lookup name rest))]))

(define (return t ret-val k)
  (type-case Cont k
    [done () (error 'interp "missing catch")]
    [numop-do-right (r ds op rest-k)
                    (return t ret-val rest-k)]
    
    [numop-do-op (l-val op rest-k)
                 (return t ret-val rest-k)]
    
    [app-do-arg (arg-expr ds rest-k)
                (return t ret-val rest-k)]
    
    [app-do-body (fun-val rest-k)
                 (return t ret-val rest-k)]
    
    [app-do-return (rest-k)
                   (return t ret-val rest-k)]
    
    [do-early-return (tagd rest-k)
                     (return t ret-val rest-k)]
    [dof (two three ds rest-k)
         (return t ret-val rest-k)]
    [catch? (tag ex exp ds rest-k)
            (if (equal? t tag)
                (interp exp
                        [aSub
                         ex
                         ret-val
                         ds]
                        rest-k)
                (return t ret-val rest-k))]
    
   
    ))

;; numop : (number? number? -> number?) EFAE? EFAE? DefSub? Cont? -> EFAE-Value?
(define (numop op l r ds k)
  (interp l ds
          (numop-do-right r ds op k)))

(define (log-interp-cont v k)
  
  ;(printf "interp-cont:\nv: ~a\nk: ~a\n\n"
  (pretty-format v)
  (pretty-format k))

;; interp-cont : EFAE-Value? Cont? -> EFAE-Value?
(define (interp-cont last-computed-val k)
  (log-interp-cont last-computed-val k)
  (type-case Cont k
    [done () last-computed-val]
    [numop-do-right (r ds op rest-k)
                    (define l-val last-computed-val)
                    (interp r ds
                            (numop-do-op l-val op rest-k))]
    [numop-do-op (l-val op rest-k)
                 (define r-val last-computed-val)
                 (unless (numV? l-val)
                   (error 'interp "expected number"))
                 (unless (numV? r-val)
                   (error 'interp "expected number"))
                 (interp-cont (numV (op (numV-n l-val) (numV-n r-val)))
                              rest-k)]
    [app-do-arg (arg-expr ds rest-k)
                (define fun-val last-computed-val)
                (interp arg-expr ds
                        (app-do-body fun-val rest-k))]
    [app-do-body (fun-val rest-k)
                 (define arg-val last-computed-val)
                 (type-case EFAE-Value fun-val
                   [closureV (param-name body closed-ds)
                             (interp body (aSub param-name
                                                arg-val
                                                closed-ds)
                                     (app-do-return rest-k))]
                   [else (error 'interp "expected function, got ~a" fun-val)])]
    [app-do-return (rest-k)
                   (define ret-val last-computed-val)
                   (interp-cont ret-val rest-k)]
    [do-early-return (t rest-k)
                     (define ret-val last-computed-val)
                     
                     (return t ret-val rest-k)]
    [catch? (t ex exp ds rest-k)
            (define ret-val last-computed-val)
            (interp-cont ret-val rest-k)]
    [dof (two three ds rest-k)
         (define ret-val last-computed-val)
         (if (equal? (numV 0) ret-val)
             (interp two ds rest-k)
             (interp three ds rest-k))]
    ))

(require racket/trace)
;(trace interp)
;(trace interp-cont)
;(trace return)

;; ----------------------------------------------------------------------
(test (interp-expr (parse `(try (+ 1 (throw x 2)) (catch (tag x as e) e))))
      2)
(test (interp-expr
       (parse `{if0 1 20 35})) 35)
(test (interp-expr
       (parse `{if0 0 20 35})) 20)
(test (interp-expr
       (parse
        `(try
          (try
           (if0
            (+
             2
             (try
              ((fun (f) (f 3)) (fun (x) (throw y 1)))
              (catch (tag a as e) (+ 5 e))))
            100
            101)
           (catch (tag z as e) (+ e 2)))
          (catch (tag y as e) (+ 10 e)))))
      11)
(test (interp-expr (parse `(+ (if0 0 (+ 10 6) 2) 5))) 21)
(test (interp-expr (parse `{+ 2 {try {+ 4 {throw x 5}}
                                     {catch {tag x as e} {+ 3 e}}}}))
      10)
(test (interp-expr (parse `{try {+ 2 {try {+ 3 {throw y 5}}
                                          {catch {tag x as e} {+ 6 e}}}}
                                {catch {tag y as e} {+ 10 e}}}))
      15)
(test/exn (interp-expr (parse `{try {throw a 1} {catch {tag a as b} {throw a 1}}}))
          "missing catch")

;you can translate `with` as usual
(test (interp-expr (parse `{with {f {fun {x} {throw a {+ x 1}}}}
                                 {try {throw a {+ {f 3} 10}}
                                      {catch {tag a as j} {+ j 5}}}}))
      9)










;; ----------------------------------------------------------------------

(test (interp-test `{fun {x} {+ x 1}})
      (closureV 'x (add (id 'x) (num 1))
                (mtSub)))
(test (interp-test `{with {y 3} {fun {x} {+ x y}}})
      (closureV 'x (add (id 'x) (id 'y))
                (aSub 'y (numV 3) (mtSub))))
(test (interp-test `{{with {y 3} {fun {x} {+ x y}}}
                     5})
      (numV 8))
(test (interp-test `{with {y 100}
                          {{with {y 3} {fun {x} {+ x y}}}
                           5}})
      (numV 8))

(test/exn (interp-test `{with {z {fun {x} {+ x y}}}
                              {with {y 10}
                                    {z 3}}})
          "free identifier")
;; A: 13 -- wrong
;; B: free identifier -- right

;; ----------

;; 5 -> 5
(test (interp-test `5)
      (numV 5))
;; {+ 1 2} -> 3
(test (interp-test `{+ 1 2})
      (numV 3))
;; {- 3 4} -> -1
(test (interp-test `{- 3 4})
      (numV -1))
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp-test `{+ {+ 1 2} {- 3 4}})
      (numV 2))

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp-test `{with {x {+ 1 2}}
                          {+ x x}})
      (numV 6))
#|
x
|#
(test/exn (interp-test `z)
          "free identifier")
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp-test `{+ {with {x {+ 1 2}}
                             {+ x x}}
                       {with {x {- 4 3}}
                             {+ x x}}})
      (numV 8))
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp-test `{+ {with {x {+ 1 2}}
                             {+ x x}}
                       {with {y {- 4 3}}
                             {+ y y}}})
      (numV 8))
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp-test `{with {x {+ 1 2}}
                          {with {x {- 4 3}}
                                {+ x x}}})
      (numV 2))
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp-test `{with {x {+ 1 2}}
                          {with {y {- 4 3}}
                                {+ x x}}})
      (numV 6))

;; ----------

(test (interp-test `{with {f {fun {x} {+ x 1}}}
                          {f 3}})
      (numV 4))
(test (interp-test `{{fun {x} {+ x 1}} 3})
      (numV 4))
(test (interp-test `{fun {x} {+ x 1}})
      (closureV 'x (parse `{+ x 1}) (mtSub)))
(test/exn (interp-test `{1 2})
          "expected function")
(test/exn (interp-test `{+ 1 {fun {x} x}})
          "expected number")
(test (interp-test `{with {f {with {x 3}
                                   {fun {y} {+ x y}}}}
                          {f 2}})
      (numV 5))