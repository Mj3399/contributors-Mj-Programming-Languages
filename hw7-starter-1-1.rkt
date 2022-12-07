#lang plaitypus


(print-only-errors #t)
;; you may use these definitions in your solution
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
(define-type TLFAE
  [num (n : number)]
  [bool (b : boolean)]
  [add (l : TLFAE) (r : TLFAE)]
  [sub (l : TLFAE) (r : TLFAE)]
  [eql (l : TLFAE) (r : TLFAE)]
  [id (name : symbol)]
  [ifthenelse (tst : TLFAE) (thn : TLFAE) (els : TLFAE)]
  [fun (arg : symbol) (typ : Type) (body : TLFAE)]
  [app (rator : TLFAE) (rand : TLFAE)]
  [consl (fst : TLFAE) (rst : TLFAE)]
  [firstl (lst : TLFAE)]
  [restl (lst : TLFAE)]
  [nil (typ : Type)]
  [mtl? (lst : TLFAE)]
  [makevector (size : TLFAE) (initial : TLFAE)]
  [set (vec : TLFAE) (index : TLFAE) (val : TLFAE)]
  [lengthl (col : TLFAE)]
  [get (col : TLFAE) (index : TLFAE)])



(define-type Type
  [numberT]
  [booleanT]
  [arrowT (dom : Type) (codom : Type)]
  [listT (typ : Type)]
  [vectorT (typ : Type)])

(define-type TypeEnv
  [mtEnv]
  [aBind (name : symbol)
         (type : Type)
         (rest : TypeEnv)])

(define lookup-type : (symbol TypeEnv -> Type)
  (lambda (name gamma)
    (type-case TypeEnv gamma
      [mtEnv () (error 'typecheck "free identifier")]
      [aBind (name2 type rest)
             (if (equal? name name2)
                 type
                 (lookup-type name rest))])))

(define parse : (s-expression -> TLFAE)
  (lambda (s-exp)
    (cond
      [(s-exp-number? s-exp)
       (num (s-exp->number s-exp))]
      [(s-exp-symbol? s-exp)
       (case (s-exp->symbol s-exp)
         [(true)  (bool #t)]
         [(false) (bool #f)]
         [else (id (s-exp->symbol s-exp))])]
      [(s-exp-list? s-exp)
       (define as-list (s-exp->list s-exp))
       (cond [(empty? as-list)
              (error 'parse "the empty list is not a valid TLFAE")]
             [(s-exp-symbol? (first as-list))
              (case (s-exp->symbol (first as-list))
                [(+)
                 (check-pieces as-list "add" 3)
                 (add (parse (second as-list))
                      (parse (third as-list)))]
                [(-)
                 (check-pieces as-list "sub" 3)
                 (sub (parse (second as-list))
                      (parse (third as-list)))]
                [(=)
                 (check-pieces as-list "eql" 3)
                 (eql (parse (second as-list))
                      (parse (third as-list)))]
                [(if)
                 (check-pieces as-list "if" 4)
                 (ifthenelse (parse (second as-list))
                             (parse (third as-list))
                             (parse (fourth as-list)))]
                [(fun)
                 (check-pieces as-list "fun" 3)
                 (unless (s-exp-list? (second as-list))
                   (error 'parse "expected parameter list"))
                 (define param-list (s-exp->list (second as-list)))
                 (check-pieces param-list "parameter list" 3)
                 (unless (s-exp-symbol? (first param-list))
                   (error 'parse "expected symbol as parameter name"))
                 (unless (and (s-exp-symbol? (second param-list))
                              (equal? ': (s-exp->symbol (second param-list))))
                   (error 'parse "expected `:`"))
                 (fun (s-exp->symbol (first param-list))
                      (parse-type (third param-list))
                      (parse (third as-list)))]
                [(cons)
                 (check-pieces as-list "cons" 3)
                 (consl (parse (second as-list))
                        (parse (third as-list)))]
                [(first)
                 (check-pieces as-list "first" 2)
                 (firstl (parse (second as-list)))]
                [(rest)
                 (check-pieces as-list "rest" 2)
                 (restl (parse (second as-list)))]
                [(nil)
                 (check-pieces as-list "nil" 2)
                 (nil (parse-type (second as-list)))]
                [(empty?)
                 (check-pieces as-list "empty?" 2)
                 (mtl? (parse (second as-list)))]
                [(make-vector)
                 (check-pieces as-list "make-vector" 3)
                 (makevector (parse (second as-list))
                             (parse (third as-list)))]
                [(set)
                 (check-pieces as-list "set" 4)
                 (set (parse (second as-list))
                      (parse (third as-list))
                      (parse (fourth as-list)))]
                [(length)
                 (check-pieces as-list "length" 2)
                 (lengthl (parse (second as-list)))]
                [(get)
                 (check-pieces as-list "get" 3)
                 (get (parse (second as-list))
                      (parse (third as-list)))]
                [else (parse-app as-list)])]
             [else (parse-app as-list)])]
      [else
       (error 'parse "expected TLFAE")])))

(define parse-app : ((listof s-expression) -> TLFAE)
  (lambda (s-exps)
    (check-pieces s-exps "app" 2)
    (app (parse (first  s-exps))
         (parse (second s-exps)))))

(define parse-type : (s-expression -> Type)
  (lambda (s-exp)
    (cond [(and (s-exp-symbol? s-exp)
                (equal? 'number (s-exp->symbol s-exp)))
           (numberT)]
          [(and (s-exp-symbol? s-exp)
                (equal? 'boolean (s-exp->symbol s-exp)))
           (booleanT)]
          [(s-exp-list? s-exp)
           (define as-list (s-exp->list s-exp))
           (case (length as-list)
             [(2)
              (unless (s-exp-symbol? (first as-list))
                (error 'parse "expected `listof` or `vectorof`"))
              (case (s-exp->symbol (first as-list))
                [(listof)
                 (listT (parse-type (second as-list)))]
                [(vectorof)
                 (vectorT (parse-type (second as-list)))]
                [else
                 (error 'parse "expected `listof` or `vectorof`")])]
             [(3)
              (unless (and (s-exp-symbol? (second as-list))
                           (equal? '-> (s-exp->symbol (second as-list))))
                (error 'parse "expected `->`"))
              (arrowT (parse-type (first as-list))
                      (parse-type (third as-list)))]
             [else (error 'parse-type "expected type")])]
          [else (error 'parse-type "expected type")])))

(define check-pieces : ((listof s-expression) string number -> void)
  (lambda (s-exps expected n-pieces)
    (unless (= n-pieces (length s-exps))
      (error 'parse
             (string-append (string-append "expected " expected)
                            (string-append " got " (to-string s-exps)))))))
(define typecheck-expr : (TLFAE -> Type)
  (lambda (TFAE)
    (typecheck TFAE (mtEnv))))

(define typecheck : (TLFAE TypeEnv -> Type)
  (lambda (a-tlfae gamma)
    (type-case TLFAE a-tlfae
      [bool (b) (booleanT)]
      [num (n)
           #|
           ------------------
           Γ ⊢ <num> : number
           |#
           (numberT)]
      [add (l r)
           #|
           Γ ⊢ e_1 : number        Γ ⊢ e_2 : number
           ----------------------------------------
                   Γ ⊢ {+ e_1 e_2} : number
           |#
           (unless (numberT? (typecheck l gamma))
             (error 'typecheck "expected number"))
           (unless (numberT? (typecheck r gamma))
             (error 'typecheck "expected number"))
           (numberT)]
      [sub (l r)
           #|
           Γ ⊢ e_1 : number        Γ ⊢ e_2 : number
           ----------------------------------------
                   Γ ⊢ {- e_1 e_2} : number
           |#
           (unless (numberT? (typecheck l gamma))
             (error 'typecheck "expected number"))
           (unless (numberT? (typecheck r gamma))
             (error 'typecheck "expected number"))
           (numberT)]
      [id  (name)
           #|
           -----------------------------
           [... <id> ← τ ...] ⊢ <id> : τ
           |#
           (lookup-type name gamma)]
      [fun (param-name param-type body)
           #|
                   Γ[<id> ← τ_1] ⊢ e : τ_2
           --------------------------------------
           Γ ⊢ {fun {<id> : τ_1} e} : (τ_1 → τ_2)
           |#
           (arrowT param-type
                   (typecheck body (aBind param-name param-type
                                          gamma)))]
      [app (fun-expr arg-expr)
           #|
           Γ ⊢ e_1 : (τ_2 → τ_3)          Γ ⊢ e_2 : τ_2
           --------------------------------------------
                       Γ ⊢ {e_1 e_2} : τ_3
           |#
           (type-case Type (typecheck fun-expr gamma)
             [arrowT (param-type result-type)
                     (define arg-type (typecheck arg-expr gamma))
                     (unless (equal? arg-type param-type)
                       (error 'typecheck "type mismatch"))
                     result-type]
             [else (error 'typecheck "type mismatch")])]
      [eql (l r)
           (type-case Type (typecheck l gamma)
             [numberT ()
                   (type-case Type (typecheck r gamma)
                     [numberT () (booleanT)]
                     [else (error 'typecheck "expected number")])]
             [else (error 'typecheck "expected number")])]
      [ifthenelse (tst thn els)
                  (type-case Type (typecheck tst gamma)
                    [booleanT ()
                           (if (equal? (typecheck thn gamma)
                                       (typecheck els gamma))
                               (typecheck thn gamma)
                               (error 'typecheck "type mismatch"))]
                    [else (error 'typecheck "expected boolean")])]
      [consl (fst rst)
             (type-case Type (typecheck rst gamma)
               [listT (restyp)
                      (if (equal? restyp (typecheck fst gamma))
                          (listT restyp)
                          (error 'typecheck "type mismatch"))]
               [else (error 'typecheck "expected list")])]
      [firstl (lst)
              (type-case Type (typecheck lst gamma)
                [listT (firstyp)
                       firstyp]
                [else 
                 (error 'typecheck "expected list")])]
      [restl  (lst)
              (type-case Type (typecheck lst gamma)
                [listT (restyp)
                       (listT restyp)]
                [else 
                 (error 'typecheck "expected list")])]
      [nil (typ) (listT typ)]
      [mtl? (lst)
            (type-case Type (typecheck lst gamma)
              [listT (restyp)
                     (booleanT)]
              [else (error 'typecheck "expected list")])]
      [makevector (length num)
                  (type-case Type (typecheck length gamma)
                    [numberT ()
                             (vectorT (typecheck num gamma))]
                    [else (error 'typecheck "expected number")]) ]
      [set (vec in thing)
           (type-case Type (typecheck vec gamma)
             [vectorT (vectyp)
                      (type-case Type (typecheck in gamma)
                        [numberT ()
                                 (if (equal? (typecheck thing gamma)
                                             vectyp)
                                     vectyp
                                 (error 'typecheck "type mismatch"))]
                        [else (error 'typecheck "expected number")])]
                      [else (error 'typecheck "expected vector")])]
      [lengthl (lst)
               (type-case Type (typecheck lst gamma)
                 [listT (lstyp) (numberT)]
                 [vectorT (vectyp) (numberT)]
                 [else (error 'typecheck "expected list or vector")])]
      [get (lst in)
           (type-case Type (typecheck in gamma)
                 [numberT () (type-case Type (typecheck lst gamma)
                 [listT (lstyp) lstyp]
                 [vectorT (vectyp) vectyp]
                 [else (error 'typecheck "expected list or vector")])]
                 
                 [else (error 'typecheck "expected number")])
           ]
      )))

;; ----------------------------------------------------------------------

(test (typecheck (parse `5)
                 (mtEnv))
      (numberT))

(test (typecheck (parse `{+ 2 3})
                 (mtEnv))
      (parse-type `number))
(test (typecheck (parse `{- 2 3})
                 (mtEnv))
      (parse-type `number))
(test (typecheck (parse `{+ 1 {- 2 3}})
                 (mtEnv))
      (parse-type `number))

(test (typecheck (parse `{fun {x : number} {+ x 5}})
                 (mtEnv))
      (parse-type `(number -> number)))
(test (typecheck (parse `{{fun {x : number} {+ x 5}}
                          5})
                 (mtEnv))
      (parse-type `number))

(test/exn (typecheck (parse `{+ 1 {fun {x : number} {+ x 5}}})
                     (mtEnv))
          "expected number")



(test (typecheck (parse `{fun {f : (number -> number)}
                              {fun {x : number} {f x}}})
                 (mtEnv))
      (parse-type `((number -> number)
                    ->
                    (number -> number))))
(test/exn (typecheck (parse `{{fun {f : (number -> number)}
                                   {fun {x : number} {f x}}}
                              3})
                     (mtEnv))
          "type mismatch")
(test (typecheck (parse `{{fun {f : (number -> number)}
                               {fun {x : number} {f x}}}
                          {fun {x : number} {+ x 5}}})
                 (mtEnv))
      (parse-type `(number -> number)))
(test/exn (typecheck (parse `{{fun {f : (number -> number)}
                                   {fun {x : number} {f x}}}
                              {fun {y : (number -> number)}
                                   {y 8}}})
                     (mtEnv))
          "type mismatch")
(test (typecheck (parse `{{{fun {f : (number -> number)}
                                {fun {x : number} {f x}}}
                           {fun {x : number} {+ x 5}}}
                          5})
                 (mtEnv))
      (numberT))

(test/exn (typecheck-expr (parse '(= 6 false)))
"expected number")

(test/exn (typecheck-expr (parse '(= true 5)))
"expected number")