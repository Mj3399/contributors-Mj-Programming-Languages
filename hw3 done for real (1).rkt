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
;<FAE> ::= <num>
;| {+ <FAE> <FAE>}
;| {- <FAE> <FAE>}
;| <id>
;| {if0 <FAE> <FAE> <FAE>}
;| {fun {<id>} <FAE>}
;| {<FAE> <FAE>} ;; application expressions

;<FWAE> ::= <num>
;| {+ <FWAE> <FWAE>}
;| {- <FWAE> <FWAE>}
;| {with {<id> <FWAE>} <FWAE>}
;| <id>
;| {if0 <FWAE> <FWAE> <FWAE>}
;| {fun {<id>*} <FWAE>}
;| {<FWAE> <FWAE>*} ;; application expressions



(define-type FWAE 
  [W-num  (n number?)]
  [W-add  (lhs FWAE?)
          (rhs FWAE?)]
  [W-sub  (lhs FWAE?)
          (rhs FWAE?)]
  [W-with (name symbol?)
          (named-expr FWAE?)
          (body FWAE?)]
  [W-id   (name symbol?)]
  [W-if0 (tst FWAE?)
         (thn FWAE?)
         (els FWAE?)]
  [W-fun  (param-name (listof symbol?))
          (body FWAE?)]
  [W-app  (fun-expr FWAE?)
          (arg-exprs (listof FWAE?))])

(define-type FAE
  [num  (n number?)]
  [add  (lhs FAE?)
        (rhs FAE?)]
  [sub  (lhs FAE?)
        (rhs FAE?)]
  [id   (name symbol?)]
  [if0 (test FAE?) (then FAE?) (else FAE?)]
  [fun  (param symbol?)
        (body FAE?)]
  [app  (fun FAE?)
        (arg FAE?)])

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body FAE?)
            (ds DefSub?)])

(define-type DefSub
  [mtSub]
  [aSub (name  symbol?)
        (value FAE-Value?)
        (rest  DefSub?)])

;; parse : s-expr -> FWAE?
(define (parse s-expr)
  (cond [(number? s-expr)
         (W-num s-expr)]
        [(symbol? s-expr)
         (W-id s-expr)]
        [(list? s-expr)
         (when (empty? s-expr)
           (error 'parse "the empty list is not a valid FWAE"))
         (case (first s-expr)
           [(+)
            (check-pieces s-expr 3 "+")
            (W-add (parse (second s-expr))
                   (parse (third s-expr)))]
           [(-)
            (check-pieces s-expr 3 "-")
            (W-sub (parse (second s-expr))
                   (parse (third s-expr)))]
           [(with)
            (check-pieces s-expr 3 "with")
            (check-pieces (second s-expr) 2 "with binding pair")
            (unless (symbol? (first (second s-expr)))
              (error 'parse "expected variable name, got ~a" (first (second s-expr))))
            (W-with (first (second s-expr))
                    (parse (second (second s-expr)))
                    (parse (third s-expr)))]
           [(if0)
            (check-pieces s-expr 4 "if0")
            (W-if0 (parse (second s-expr))
                   (parse (third s-expr))
                   (parse (fourth s-expr)))
            ]
           [(fun)
            (check-pieces s-expr 3 "fun")
            ;(check-pieces (second s-expr) 1 "parameter list")
            (W-fun (second s-expr)
                   
                   (parse (third s-expr)))]
           [else
            ;(check-pieces s-expr 2 "app")
            (W-app (parse (first s-expr)) (map parse (rest s-expr)))])]
        [else
         (error 'parse "expected FWAE, got ~a" s-expr)]))

(define (check-pieces s-expr n who)
  (unless (and (list? s-expr) (= (length s-expr) n))
    (error 'parse "expected ~a, got ~a" who s-expr)))

;; ----------------------------------------------------------------------------

;; compile : FWAE? -> FAE?
(define (compile an-fwae)
  (type-case FWAE an-fwae
    [W-num (n) (num n)]
    [W-add (l r)
           (define l-compiled (compile l))
           (define r-compiled (compile r))
           (if (and (num? l-compiled)
                    (num? r-compiled))
               (num (+ (num-n l-compiled)
                       (num-n r-compiled)))
               (add l-compiled r-compiled))]
    [W-sub (l r)
           (define l-compiled (compile l))
           (define r-compiled (compile r))
           (if (and (num? l-compiled)
                    (num? r-compiled))
               (num (- (num-n l-compiled)
                       (num-n r-compiled)))
               (sub l-compiled r-compiled))]
    [W-id (name) (id name)]
    [W-if0 (one two three)
           (if0 (compile one)
                (compile two)
                (compile three)
                )
           ]

    [W-fun (param-names body) (if (equal? 0 (length param-names))
                                  (error "nullary function")
                                  (if (equal? (length param-names) 1)
                                      (fun (first param-names) (compile body))
                                      (fun (first param-names) (compile (W-fun (rest param-names) body)))))]

    [W-app (fun-expr arg-exprs)
           (if (equal? 0 (length arg-exprs)) (error "nullary application") 
               (comhelp (compile fun-expr) arg-exprs
                    
                        ))]

    [W-with (name named-expr body)
            (app (fun name (compile body))
                 (compile named-expr))]))

(require racket/trace)
;(trace compile)
;app( app( f x) y )
(define (comhelp fun-expr arg-exprs)
  (if (equal? 0 (length arg-exprs))
      
      fun-expr
      
      (comhelp (app  fun-expr (compile (first arg-exprs))) 
               (rest arg-exprs))))
      
(test (compile (W-add (W-num 1) (W-num 2)))
      (num 3))
(test (compile (parse `{+ 1 2}))
      (num 3))
(test (compile (parse `{with {x 3} {+ x 2}}))
      ;; {{fun {x} {+ x 2}} 3}
      (app (fun 'x (add (id 'x) (num 2)))
           (num 3)))
(test (compile (parse `{+ 2 {with {x 3} {+ x 2}}}))
      (add (num 2)
           (app (fun 'x (add (id 'x) (num 2)))
                (num 3))))
(test (compile (parse `{with {x 3} {with {y 2} {+ x y}}}))
      (app (fun 'x (app (fun 'y (add (id 'x) (id 'y)))
                        (num 2)))
           (num 3)))

(test (compile (parse `{+ 2 2}))
      (num 4))
(test (compile (parse `{+ 2 {- 3 4}}))
      (num 1))
(test (compile (parse `{+ 2 {+ 3 x}}))
      (add (num 2) (add (num 3) (id 'x))))
(test (compile (parse `{+ x {+ 2 3}}))
      (add (id 'x) (num 5)))
(test (compile (parse `{f {+ 2 3}}))
      (app (id 'f) (num 5)))

(define (unwrap g)
  (type-case FAE-Value g
    [numV (n) n]
    [closureV (param-name body closed-ds)
              'function]))
                      
(define (interp-expr exp)
  
  (unwrap (interp exp initial-def-sub))
          
  )

;; interp : FAE? DefSub? -> FAE-Value?
(define (interp an-fae ds)
  (type-case FAE an-fae
    [num (n) (numV n)]
    [add (l r) (numop + l r ds)]
    [sub (l r) (numop - l r ds)]
    [id (name) (lookup name ds)]
    [fun (param-name body) (closureV param-name body ds)]
    [if0 (one two three)
         (if (equal? (numV 0) (interp one ds))
             (interp two ds)
             (interp three ds))]
    [app (fun-expr arg-expr)
         (define fun-val (interp fun-expr ds))
         (define arg-val (interp arg-expr ds))
         (type-case FAE-Value fun-val
           [closureV (param-name body closed-ds)
                     (interp body
                             (aSub param-name
                                   arg-val
                                   closed-ds))]
           ;[numV (n) n]
           [else (error 'interp "expected function, got ~a" fun-val)]
           )]))

(define (numop op l r ds)
  (define l-val (interp l ds))
  (define r-val (interp r ds))
  (unless (and (numV? l-val) (numV? r-val))
    (error 'interp "expected number, got ~a" r-val))
  (numV (op (numV-n l-val) (numV-n r-val))))

;; lookup : symbol? DefSub? -> FAE-Value?
(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () ;(print name)
           (error 'interp "free identifier, got ~a" name)]
    [aSub (name2 val rest)
          (if (equal? name name2)
              val
              (lookup name rest))]))

;; ----------------------------------------------------------------------------


(define initial-def-sub (mtSub))

(test/exn (interp-expr (compile (parse `{+ {fun {x} x}
                                           {1 2}})))
          "expected function")

(test (interp-expr (num 10)) 10)
(test (interp-expr (fun 'x (id 'x))) 'function)
(test (interp-expr
       (compile (parse `{if0 1 20 35}))) 35)
(test (interp (compile (parse `{if0 0 1 2})) '()) (numV 1))
(test (interp (compile (parse `{if0 1 2 3})) '()) (numV 3))
(test (interp (compile (parse `{if0 0 9 10}))  initial-def-sub) (numV 9))
(test (interp (compile (parse `{if0 1 20 35}))  initial-def-sub) (numV 35))
;; 5 -> 5
(test (interp (compile (parse `5))
              initial-def-sub)
      (numV 5))
;; {+ 1 2} -> 3
(test (interp (compile (parse `{+ 1 2}))
              initial-def-sub)
      (numV 3))
;; {- 3 4} -> -1
(test (interp (compile (parse `{- 3 4}))
              initial-def-sub)
      (numV -1))
;; {+ {+ 1 2} {- 3 4}} -> 2
(test (interp (compile (parse `{+ {+ 1 2} {- 3 4}}))
              initial-def-sub)
      (numV 2))

#|
{with {x {+ 1 2}}
      {+ x x}}
|#
(test (interp (compile (parse `{with {x {+ 1 2}}
                                     {+ x x}}))
              initial-def-sub)
      (numV 6))
(test (interp (compile (parse `{with {x {+ 1 2}}
                                     {- x x}}))
              initial-def-sub)
      (numV 0))
#|
x
|#
(test/exn (interp (compile (parse `x))
                  initial-def-sub)
          "free identifier")
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {x {- 4 3}}
         {+ x x}}}
|#
(test (interp (compile (parse `{+ {with {x {+ 1 2}}
                                        {+ x x}}
                                  {with {x {- 4 3}}
                                        {+ x x}}}))
              initial-def-sub)
      (numV 8))
#|
{+ {with {x {+ 1 2}}
         {+ x x}}
   {with {y {- 4 3}}
         {+ y y}}}
|#
(test (interp (compile (parse `{+ {with {x {+ 1 2}}
                                        {+ x x}}
                                  {with {y {- 4 3}}
                                        {+ y y}}}))
              initial-def-sub)
      (numV 8))
#|
{with {x {+ 1 2}}
      {with {x {- 4 3}}
            {+ x x}}}
|#
(test (interp (compile (parse `{with {x {+ 1 2}}
                                     {with {x {- 4 3}}
                                           {+ x x}}}))
              initial-def-sub)
      (numV 2))
#|
{with {x {+ 1 2}}
      {with {y {- 4 3}}
            {+ x x}}}
|#
(test (interp (compile (parse `{with {x {+ 1 2}}
                                     {with {y {- 4 3}}
                                           {+ x x}}}))
              initial-def-sub)
      (numV 6))

(test (interp (compile (parse `{with {f {fun {x} {+ x 1}}}
                                     {f 3}}))
              initial-def-sub)
      (numV 4))

;two arg interp
(test (interp (compile (parse `{with {f {fun {x y} {+ x y}}}
                                     {f 3 3}}))
              initial-def-sub)
      (numV 6))
;(trace compile)
(test (compile (parse `{with {f {fun {x y} {+ x y}}}
                             {f 3 3}}))
      {app {fun 'f (app (app (id 'f) (num 3)) (num 3))} {fun 'x {fun 'y (add (id 'x) (id 'y))}} } )
(test (compile (parse `{fun {a b} {+ a b}}))
      {fun 'a {fun 'b {add {id 'a} {id 'b}}}})
(test (compile (parse `{f x y z}))
      {app {app {app {id 'f} {id 'x} } {id 'y} } {id 'z}})



(test (interp (compile (parse `{{fun {x} {+ x 1}} 3}))
              initial-def-sub)
      (numV 4))

(test (interp (compile (parse `{fun {x} {+ x 1}}))
              initial-def-sub)
      (closureV 'x (add (id 'x) (num 1)) (mtSub)))

(test/exn (interp (compile (parse `{1 2}))
                  initial-def-sub)
          "expected function")

(test/exn (interp (compile (parse `{+ 1 {fun {x} 10}}))
                  initial-def-sub)
          "expected number")

(test (interp (compile (parse `{with {f {with {x 3}
                                              {fun {y} {+ x y}}}}
                                     {f 5}}))
              initial-def-sub)
      (numV 8))

(test/exn (interp (compile (parse `{with {z {fun {x} {+ x y}}}
                                         {with {y 10} {z y}}}))
                  initial-def-sub)
          "free identifier")

(test/exn (interp (compile (parse `{+ {fun {x} x}
                                      {1 2}})) initial-def-sub)
          "expected function")



(define mult
  '{with {mult
          {fun {x y}
               {with {multx
                      {fun {multx}
                           {fun {x}
                                {fun {y}
                                     {if0 y
                                          0
                                          {+ x {{{multx multx} x} {- y 1}}}}}}}}
                     {{{multx multx} x} y}}}}
         {fun {x y} {mult x y}}}
  )
(define factorial
  `{with {fac
          {fun {n}
               {with {facx
                      {fun {facx n}
                           {if0 n
                                1
                                {,mult n {facx facx {- n 1}}}}}}
                     {facx facx n}}}}
         fac}
  )

(test (interp-expr (compile (parse `{, factorial 3})) )  6)
(test (interp-expr (compile (parse `{, mult 3 2})) )  6)

;{deffun {neg? x} {if0 x 1 {neghelp x x}}};
;`{deffun {neghelp x y} {if0 x 0 {if0 y 1 {neghelp {+ x 1} {- y 1}}}}
;// outside def for neg
(define neg
  '{with {neg
       {fun {x}
         {with {negx
                {fun {negx}
                  {fun {x}
                       {fun {y}
                        {if0 y
                        1
                        {if0 x
                             0
                             {{{negx negx} {+ 1 x}} {- y 1}}}}}}}}
           {{{negx negx} x} x}}}}
      {fun {x} {neg x}}}
  )


;// outside def for division
   (define div
     '{with {neg
       {fun {x}
         {with {negx
                {fun {negx}
                  {fun {x}
                       {fun {y}
                        {if0 y
                        1
                        {if0 x
                             0
                             {{{negx negx} {+ 1 x}} {- y 1}}}}}}}}
           {{{negx negx} x} x}}}}
     {with {div
             {fun {x y z}
                  {with {divx
                         {fun {divx}
                              {fun {x}
                                   {fun {y}
                                        {fun {z}
                                          {if0 {- x y}
                                               {+ z 1}
                                               {if0  {neg {- x y}}
                                                    0
                                                    
                                               {{{{divx divx} {- x y}} y} {+ z 1}}}
                                               }}}}}}
                         {{{{divx divx} x} y}z}}}}
            {fun {x y z} {div x y z}}}}
     )

;//final binding for prime with no unquotes just adapted outside defs into inside defs 
   (define prime?
     '{with {neg
       {fun {x}
         {with {negx
                {fun {negx}
                  {fun {x}
                       {fun {y}
                        {if0 y
                        1
                        {if0 x
                             0
                             {{{negx negx} {+ 1 x}} {- y 1}}}}}}}}
           {{{negx negx} x} x}}}}
     {with {div
             {fun {x y z}
                  {with {divx
                         {fun {divx}
                              {fun {x}
                                   {fun {y}
                                        {fun {z}
                                          {if0 {- x y}
                                               {+ z 1}
                                               {if0  {neg {- x y}}
                                                    0
                                                    
                                               {{{{divx divx} {- x y}} y} {+ z 1}}}
                                               }}}}}}
                         {{{{divx divx} x} y}z}}}}
            
           {with {prime
             {fun {x}
                  {with {prix
                         {fun {prix}
                              {fun {x}
                                   {fun {y}
                                        {if0 {- y 1}
                                                0
                                                {if0 {div x y 0}
                                                     {{{prix prix} x} {- y 1}}
                                                     1
                                           
                                                     }}}}}}
                        {{{prix prix} x} {- x 1}}}}}
           {fun {x} {prime x}}}}}
     )
(test (interp-expr (compile (parse `{, neg 2}))) 1)
(test (interp-expr (compile (parse `{, neg -2}))) 0)
   (test (interp-expr (compile (parse `{, div 2 2 0}))) 1)
(test (interp-expr (compile (parse `{, div 3 3 0}))) 1)
(test (interp-expr (compile (parse `{, div 6 3 0}))) 2)
(test (interp-expr (compile (parse `{, div 10 2 0}))) 5)
(test (interp-expr (compile (parse `{, div 10 4 0}))) 0)
(test (interp-expr (compile (parse `{, div 10 3 0}))) 0)
(test (interp-expr (compile (parse `{, div 10 5 0}))) 2)
(test (interp-expr (compile (parse `{, div 0 1 0}))) 0)
   (test (interp-expr (compile (parse `{, prime? 11}))) 0)
(test (interp-expr (compile (parse `{, div 1 1 0}))) 1)
(test (interp-expr (compile (parse `{, div 2 1 0}))) 2)
(test (interp-expr (compile (parse `{, div 200 1 0}))) 200)
(test (interp-expr (compile (parse `{, prime? 10}))) 1)

(test (interp-expr (compile (parse `{, prime? 97}))) 0)
(test (interp-expr (compile (parse `{, prime? 98}))) 1)
;(trace interp)
   (test (interp-expr (compile (parse `{, factorial 10})) )  3628800)
   ;(test (interp-expr (compile (parse `{, factorial 11})) )  39916800) out of memory
   (test (interp-expr (compile (parse `{, factorial 7})) )  5040)
   (test (interp-expr (compile (parse `{, prime? 29}))) 0)
   (test (interp-expr (compile (parse `{, prime? 10}))) 1)
   