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
(define-type RFAE
  [num (n number?)]
  [add (lhs RFAE?)
       (rhs RFAE?)]
  [sub (lhs RFAE?)
       (rhs RFAE?)]
  [id (name symbol?)]
  [fun (param-name symbol?)
       (body RFAE?)]
  [app (fun-expr RFAE?)
       (arg-expr RFAE?)]
  [rstruct (thing
            (listof pair?))]  
  [get (expr RFAE?)
       (id symbol?)]
  [rset (expr RFAE?)
        (id symbol?)
        (val RFAE?)]
  [seqn (expr1 RFAE?)
        (expr2 RFAE?)])



(define-type Value*Store
  [v*s (v RFAE-Value?)
       (s Store?)])



(define-type Store
  [mtSto]
  [aSto (address integer?)
        (id symbol?)
        (v RFAE-Value?)
        (rest Store?)])

(define-type RFAE-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body RFAE?)
            (ds DefSub?)]
  [boxV (address integer?)])

(define-type DefSub
  [mtSub]
  [aSub  (name symbol?)
         (value RFAE-Value?)
         (rest DefSub?)])
;help functions
(define (interp-two e1 e2 ds st finish)
  (type-case Value*Store (interp e1 ds st)
    [v*s (v1 st2)
         (type-case Value*Store (interp e2 ds st2)
           [v*s (v2 st3)
                (finish v1 v2 st3)])]))
;(trace interp-expr)
(define (lookup-store address st)
  (type-case Store st
    [mtSto () (error 'interp "unknown field")]
    [aSto (a2 id v r)
     
          (if (equal? a2 address)
              st
              (lookup-store address r))]))
;what if we just want value then
(define (lookup-storev namae st)
  (type-case Store st
    [mtSto () (error 'interp "unknown field")]
    [aSto (a2 id v r)
     
          (if (equal? namae id)
              v
              (lookup-storev namae r))]))
;what if we have all the knows
(define (ls a f st)
  (lookup-storev f
                 (lookup-store a st)))


;we know the what we want to put where

(define (setv f v st)
  (type-case Store st
    [aSto (a id ov rest)
          (if (equal? f id)
              (aSto a f v rest)
              (aSto a id ov
                    (setv f v rest)))]
    [else (error 'set "unknown field")]))  ;yeah this should work


;so if know every then?

(define (seta a f v st)
  (type-case Store st
    [mtSto () (error "unknown field")]
    [aSto (a2 id ov rest)
          (if (equal? a a2)
              (setv f v st)
              (aSto a2 id ov
                    (seta a f v rest)))]))
;combo mode
(define (setE a f v st)
  (seta a f v st))


(define (malloc st)
  (+ 1 (max-address st)))
(define (max-address st)
  (type-case Store st
    [mtSto () -1]
    [aSto (a id v r) (max a (max-address r))])) ;recursion



;; numop : (number? number? -> number?)
;;         RFAE? RFAE? DefSub? Store? -> Value*Store?
(define (numop op l r ds st)
  (interp-two
   l r ds st
   (lambda (l-val r-val st3)
     (unless (numV? l-val)
       (error 'interp "expected number, got ~a" l-val))
     (unless (numV? r-val)
       (error 'interp "expected number, got ~a" r-val))
     (v*s (numV (op (numV-n l-val) (numV-n r-val)))
          st3))))


;if i want to make a struct then
;car first pair cdr for sec pair
(define (structing s ds st)
  (if (empty? s)
      (v*s (boxV (max-address st)) st)
      (type-case Value*Store (interp (cdr (first s)) ds st)
        [v*s (v st2)
             (structing (rest s) ds (aSto (malloc st2)
                                          (car (first s))
                                          v
                                          st2))]))) ;jesus

;doing the hard part now so we dont need to freak out later ig yeah
;learned that the hard way from the recursion hw3 doing everything all in one spot is bad for the soul

;; lookup : symbol? DefSub? -> RFAE-Value?
(define (lookup name ds st)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier")]
    [aSub (name2 val rest)
          (if (equal? name name2)
              (v*s val st)
              (lookup name rest st))]))

;simpler
(define (lookupd name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free identifier")]
    [aSub (name2 val rest)
          (if (equal? name name2)
              val
              (lookupd name rest))]))
;; ----------------------------------------------------------------------

;; parse : s-expression -> RFAE?
(define (parse s-exp)
  (cond [(number? s-exp)
         (num s-exp)]
        [(symbol? s-exp)
         (id s-exp)]
        [(list? s-exp)
         (when (empty? s-exp)
           (error 'parse "the empty list is not a valid RFAE"))
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
           [(struct)
            ;(check-pieces s-exp "rstruct" 2) ;remember name clashes but still tested with real names so only use fake name outside of input to avoid clash still need to accept real name
            (rstruct (map (λ (x) 
                            (cons (first  x) (parse (last  x)))) (rest s-exp)))] 
           [(set)
            ;(check-pieces s-exp "rset" 3)
            (rset (parse (first (rest s-exp)))
                  (second (rest s-exp))
                  (parse (third (rest s-exp))))]
           [(get)
            ;(check-pieces s-exp "get" 2)
            (get (parse (second s-exp))
                 (third s-exp))]
           [(seqn)
            ;(check-pieces s-exp "seqn" 3)
            (seqn (parse (second s-exp))
                  (parse (third s-exp)))]
           [else
            ;(check-pieces s-exp "app" 2)
            (app (parse (first s-exp))
                 (parse (second s-exp)))])]
        [else
         (error 'parse "expected RFAE got ~a" s-exp)]))

(define (check-pieces s-exp expected n-pieces)
  (unless (and (list? s-exp)
               (= n-pieces (length s-exp)))
    (error 'parse "expected ~a got ~a" expected s-exp)))

;; ----------------------------------------------------------------------

;; interp-test : s-expression -> RFAE-Value?
(define (interp-expr a-RFAE)
  (type-case Value*Store (interp a-RFAE (mtSub) (mtSto))
    [v*s (val st)
         (type-case RFAE-Value val
           [numV (n)  n]
           [closureV (param body ds) 'procedure]
           [boxV (address) 'struct])]))

;; interp : RFAE? DefSub? Store? -> Value*Store?
(define (interp a-RFAE ds st) ; NEW
  ;(printf "size ~a\n" (size st))
  (type-case RFAE a-RFAE
    [num (n) (v*s (numV n) st)]
    [add (l r) (numop + l r ds st)]
    [sub (l r) (numop - l r ds st)]
    [id (name)
        (v*s (lookupd name ds) st)]
    [fun (param-name body)
         (v*s (closureV param-name body ds)
              st)]
    [app (fun-expr arg-expr)
         (interp-two fun-expr arg-expr ds st
                     (λ (fun-val arg-val st3) ;i like that u can type the real lambda symbol with control backslash λ much nicer than writing lambda everywhere
                       (type-case RFAE-Value fun-val
                         [closureV (param-name body closed-ds)
                                   (interp body
                                           (aSub param-name
                                                 arg-val
                                                 closed-ds)
                                           st3)]
                         [else (error 'interp "expected function, got ~a" fun-val)])))]
    [rstruct (s) (structing s ds st)] ;now this, this is beautiful ;take that hw3 
    [get (box-expr field)
         (type-case Value*Store (interp box-expr ds st)
           [v*s (box-val st2)
                (type-case RFAE-Value box-val
                  [boxV (address)
                        (v*s (ls address field st2)
                             st2)]
                  [else (error 'interp "expected record, got ~a" box-val)])])]
    
    [rset (box-expr field new-expr)
          (type-case Value*Store (interp box-expr ds st)
            [v*s (val1 st1)
                 (type-case RFAE-Value val1 ;craft it by hand
                   
                   [boxV (address)
                         (type-case Value*Store (interp new-expr ds st1)
                           [v*s (val2 st2) (v*s (ls address field st2)
                                                (setE address field val2 st2))])]
                   [else (error "expected record")])])
          ]
    [seqn (expr1 expr2)
          (interp-two expr1 expr2 ds st
                      (lambda (v1 v2 st3)
                        (v*s v2 st3)))]))
(require racket/trace)



; size : any -> number?
; computes a (very rough!) approximation of
; the size a PLAI object takes in memory
(define (size s)
  (cond
    [(struct? s)
     (size (struct->vector s))]
    [(vector? s)
     (for/fold ([tot 0])
               ([ele (in-vector s)])
       (+ tot (size ele)))]
    [(pair? s)
     (+ 1 (size (car s)) (size (cdr s)))]
    [else 1]))
;tests from pdf




(test/exn (interp-expr (parse '{struct {z {get {struct {z 0}} y}}}))
          "unknown field")


(test (interp-expr (parse '{{fun {r}
                                 {get r x}}
                            {struct {x 1}}}))
      1)

(test (interp-expr (parse '{{fun {r}
                                 {seqn
                                  {set r x 5}
                                  {get r x}}}
                            {struct {x 1}}}))
      5)
(test (interp-expr (parse '{set {struct {x 42}} x 2}))
      42)
(test (interp-expr (parse '{{{{{fun {g}
                                    {fun {s}
                                         {fun {r1}
                                              {fun {r2}
                                                   {+ {get r1 b}
                                                      {seqn
                                                       {{s r1} {g r2}}
                                                       {+ {seqn
                                                           {{s r2} {g r1}}
                                                           {get r1 b}}
                                                          {get r2 b}}}}}}}}
                               {fun {r} {get r a}}} ; g
                              {fun {r} {fun {v} {set r b v}}}} ; s
                             {struct {a 0} {b 2}}} ; r1
                            {struct {a 3} {b 4}}})) ; r2
      5)
