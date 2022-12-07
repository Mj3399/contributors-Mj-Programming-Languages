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

(define-type Tree
  [positive-leaf (val natural?)]
  [negative-leaf (val natural?)]
  [interior-node (left Tree?) (right Tree?)])

(define (contains? a-tree number)
  (type-case Tree a-tree
    [positive-leaf (val) (if (equal? val number) true false) ]
    [negative-leaf (val) (if (equal? (* -1 val) number)  true false )]
    [interior-node (l r) (or (contains? l number) (contains? r number))]))

(test (contains? (interior-node (interior-node (positive-leaf 5)
                                               (negative-leaf 4))
                                (positive-leaf 3))
                 -4)
      true)
(test (contains? (interior-node (interior-node (positive-leaf 5)
                                               (negative-leaf 4))
                                (positive-leaf 3))
                 3)
      true)

(define (smalhelp atree small)
  (type-case Tree atree
    [positive-leaf (v1) (if (< small v1) small v1) ]
    [negative-leaf (v2) (if (< small (* -1 v2)) small (* -1 v2))]
    [interior-node (l r) (if (> (smalhelp l small) (smalhelp r small)) (smalhelp r small) (smalhelp l small))]))
(define (smallest atree)
  (smalhelp atree +inf.0))
(test (smallest (interior-node (interior-node (positive-leaf 5)
                                              (negative-leaf 9))
                               (positive-leaf 3))
                )
      -9)

(test (smallest 
       (positive-leaf 3)
       )
      3)
(define (balhelp atree sum)
  (type-case Tree atree
    [positive-leaf (v1) (+ sum v1) ]
    [negative-leaf (v2) (+ sum (* -1 v2))]
    [interior-node (l r) (+ (balhelp l sum) (balhelp r sum))]))

(define (balanced? atree)
  (if (equal? 0 (balhelp atree 0)) true false) )

(test (balanced? (interior-node (interior-node (positive-leaf 5)
                                               (negative-leaf 9))
                                (positive-leaf 1))
                 )
      false)

(test (balanced? (interior-node (interior-node (positive-leaf 5)
                                               (negative-leaf 9))
                                (positive-leaf 4))
                 )
      true)
(test (balanced? (interior-node (positive-leaf 0) (positive-leaf 0))) true)

(define (deephelp atree)
  (type-case Tree atree
    [positive-leaf (v1)  true]
    [negative-leaf (v2) true ]
    [interior-node (l r)  (and (balanced? [interior-node l r])
                               (if (interior-node? l) (balanced? l) true) (if (interior-node? r) (balanced? r) true))]))
                                                                  


(define (deep-balanced? atree)

  (deephelp atree))

(test (deep-balanced? (interior-node (interior-node (positive-leaf 5)
                                                    (negative-leaf 9))
                                     (positive-leaf 1))
                      )
      false)
(test (deep-balanced? (interior-node (interior-node (positive-leaf 5)
                                                    (negative-leaf 9))
                                     (positive-leaf 4))
                      )
      false)
(test (deep-balanced? (interior-node (interior-node (positive-leaf 5)
                                                    (negative-leaf 5))
                                     (interior-node (positive-leaf 2)
                                                    (negative-leaf 2)))
                      )
      true)
(test (deep-balanced? (interior-node (positive-leaf 0) (positive-leaf 0))) true)
(test (deep-balanced? (interior-node (negative-leaf 1) (positive-leaf 1))) true)
(test (deep-balanced? (interior-node (positive-leaf 1) (positive-leaf 1))) false)


(define (negate atree)
  (type-case Tree atree
    [positive-leaf (v1) [negative-leaf v1]]
    [negative-leaf (v2) [positive-leaf v2]] 
    [interior-node (l r) [interior-node (negate l) (negate r)]]
    ))

(test (negate (interior-node (interior-node (positive-leaf 5)
                                            (negative-leaf 9))
                             (positive-leaf 1))
              )
      (interior-node (interior-node (negative-leaf 5)
                                    (positive-leaf 9))
                     (negative-leaf 1)))

(define (add atree numb)
  (type-case Tree atree
    [positive-leaf (v1) (if (>= (+ v1 numb) 0) [positive-leaf (+ v1 numb)] [negative-leaf (* -1 (+ v1 numb))])]
    [negative-leaf (v2) (if (<= (+ (* -1 v2) numb) 0) [negative-leaf (- v2 numb)] [positive-leaf (+ (* -1 v2) numb)] )] 
    [interior-node (l r) [interior-node (add l numb) (add r numb)]]))

(test (add (interior-node (interior-node (positive-leaf 5)
                                         (negative-leaf 9))
                          (positive-leaf 3))
           2)
      (interior-node (interior-node (positive-leaf 7)
                                    (negative-leaf 7))
                     (positive-leaf 5)))
(test (add (interior-node (interior-node (positive-leaf 5)
                                         (negative-leaf 9))
                          (positive-leaf 3))
           10)
      (interior-node (interior-node (positive-leaf 15)
                                    (positive-leaf 1))
                     (positive-leaf 13)))
(test (add (interior-node (interior-node (positive-leaf 5)
                                         (negative-leaf 9))
                          (positive-leaf 3))
           -20)
      (interior-node (interior-node (negative-leaf 15)
                                    (negative-leaf 29))
                     (negative-leaf 17)))

(define (positive-thinking atree)
  (type-case Tree atree
    [positive-leaf (v1) [positive-leaf v1]]
    [negative-leaf (v2) false]
    [interior-node (l r) (if (and  (positive-thinking l) (positive-thinking r))
                             [interior-node (positive-thinking l) (positive-thinking r)]
                             (if  (positive-thinking l)  (positive-thinking l)
                                  (if  (positive-thinking r)  (positive-thinking r) false)))]))
 
(test (positive-thinking (interior-node (interior-node (positive-leaf 5)
                                                       (negative-leaf 9))
                                        (positive-leaf 3)))
      (interior-node (positive-leaf 5)  (positive-leaf 3)))

(test (positive-thinking (interior-node (positive-leaf 5)
                                        (negative-leaf 9))
                         )
      (positive-leaf 5) )

(test (positive-thinking 
       (negative-leaf 9))
                                 
      false )
(test (positive-thinking (positive-leaf 5)
                                               
                         )
      (positive-leaf 5) )