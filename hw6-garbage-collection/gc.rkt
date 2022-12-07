#lang plai/gc2/collector
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

;THIS IS HOMEWORK 6

#|
heap:    | 'free | 'free | 'free | ...                          NEW!
flat:    ... | 'flat | <payload>   | ...
pair:    ... | 'cons | <first-ptr> | <rest-ptr> | ...
closure: ... | 'clos | <code-ptr>  | <n-free-vars> | <fv0> | <fv1> | ... | ...
|#

(define (init-allocator)
  (heap-set! 0 4)
  (heap-set! 1 4)
  (heap-set! 2 (+ 4
                  (/ (- (heap-size) 4) 2)))
  (heap-set! 3 (+ 4
                  (/ (- (heap-size) 4) 2)))
  
  )
;;^^^ this works i am sure of it


#|
flat:    ... | 'flat | <payload> | ...
|#
(define (gc:deref address)
  (unless (gc:flat? address)
    (error 'gc:deref "expected flat @ ~a" address))
  (heap-ref (+ address 1)))

(define (gc:alloc-flat value)
  (define address (malloc 2 '()))
  (heap-set! address 'flat)
  (heap-set! (+ address 1) value)
  
  address)
(define (gc:flat? address)
  (equal? (heap-ref address) 'flat)) 

#|
pair:    ... | 'cons | <first-ptr> | <rest-ptr> | ...
|#
;; gc:cons : root root -> address


(define (gc:cons root1 root2)
  (define address (malloc 3 (list root1 root2))) 
  ;(println address)
  (heap-set! address 'cons)
  (heap-set! (+ address 1) (read-root root1))
  (heap-set! (+ address 2) (read-root root2))
  
  address)

;; gc:cons? : address -> boolean
(define (gc:cons? address)
  (equal? (heap-ref address) 'cons))
;; gc:first : address -> address
(define (gc:first address)
  (unless (gc:cons? address)
    (error 'gc:first "expected cons @ ~a" address))
  (heap-ref (+ address 1)))
;; gc:rest : address -> address
(define (gc:rest address)
  (unless (gc:cons? address)
    (error 'gc:rest "expected cons @ ~a" address))
  (heap-ref (+ address 2)))
;; gc:set-first! : address address -> void
(define (gc:set-first! address new-value-address)
  (unless (gc:cons? address)
    (error 'gc:set-first! "expected cons @ ~a" address))
  (heap-set! (+ address 1) new-value-address))
;; gc:set-rest! : address address -> void
(define (gc:set-rest! address new-value-address)
  (unless (gc:cons? address)
    (error 'gc:set-rest! "expected cons @ ~a" address))
  (heap-set! (+ address 2) new-value-address))


#|
closure: ... | 'clos | <code-ptr> | <n-free-vars> | <fv0> | <fv1> | ... | ...
|#
;; gc:closure : opaque-value (listof root) -> address
(define (gc:closure code-ptr free-vars)
  (define address (malloc (+ 3 (length free-vars))
                          free-vars))
  ;(println free-vars)
  ;(println address) 
  (heap-set! address 'clos) 
  (heap-set! (+ address 1)  (length free-vars))
  (heap-set! (+ address 2) code-ptr)
  (for ([fv (in-list free-vars)]
        [i  (in-range 0 (length free-vars))])
    (heap-set! (+ address 3 i) (read-root fv)))
 
  address)
;; gc:closure? : address -> boolean
(define (gc:closure? address)
  (equal? (heap-ref address) 'clos))
;; gc:closure-code-ptr : address -> opaque-value
(define (gc:closure-code-ptr address)
  (unless (gc:closure? address)
    (error 'gc:closure-code-ptr "expected closure @ ~a" address))
  (heap-ref (+ 2 address)))
;; gc:closure-env-ref : address integer -> address
(define (gc:closure-env-ref address i)
  (unless (gc:closure? address)
    (error 'gc:closure-env-ref "expected closure @ ~a" address))
  
  (heap-ref (+ address 3 i))
  )


(define (malloc n rootlist)
  (define F (find-n-free-spaces n))
  (case (integer? F)
    
    [(#t)
     (define address (+ n F))
     ;(println address)
     (heap-set! 1 address)
     ;(println (find-n-free-spaces n))
     F
     ]

    [(#f)
     (define sob (append (get-root-set) rootlist))
     (collect-garbage sob)
     (define FF (find-n-free-spaces n))
     (case (integer? FF)
       [(#t)
        (define address (+ n FF))
        (heap-set! 1 address)
        FF
        ]

       [(#f)
        (error 'malloc "out of memory!")]
       )
     ]
    ))
  
  

(define (collect-garbage rootlist)

  (define old (heap-ref 0))
  (define new (heap-ref 2));if you dont put them here they can get overwritten!!!!!!!!!!!
  (for-each (λ (root) (plswork root)) rootlist )
  

  (heap-set! 0 new)
  (heap-set! 1 (heap-ref 2))
  (heap-set! 2 old)
  (heap-set! 3 old)

  
  )


(define (find-n-free-spaces size)
  ;(println (heap-ref 1))
  (if (<= (+ size (heap-ref 1)) (+ (heap-ref 0)
                                   (/ (- (heap-size) 4) 2)))
      
      (heap-ref 1)
      #f))

(define (plswork root)
  ;(read-line)  
  (set-root! root (trailer (read-root root) ))  (header))


(define (trailer ptr)
  
  (define tail (heap-ref 3))
  (case (heap-ref ptr)
    
    [(flat)
     (heap-set! tail 'flat)
     (heap-set! (+ tail 1) (heap-ref (+ 1 ptr)))
     (heap-set! ptr 'beenheredonethat)
     (heap-set! (+ 1 ptr) tail)
     (heap-set! 3 (+ 2 tail))
     tail]
    [(cons)
     (heap-set! tail 'cons)
     (heap-set! (+ tail 1) (heap-ref (+ 1 ptr)))
     (heap-set! (+ tail 2) (heap-ref (+ 2 ptr)))
     (heap-set! ptr 'beenheredonethat)
     (heap-set! (+ 1 ptr) tail)
     (heap-set! 3 (+ 3 tail))
     tail]
    [(clos)
     (define die (heap-ref (+ ptr 1))) ;possible overwrite other?
     (heap-set! tail 'clos)
     (heap-set! (+ tail 1) (heap-ref (+ 1 ptr)))
     (heap-set! (+ tail 2) (heap-ref (+ 2 ptr)))
     (for ([i (in-range 0 (heap-ref (+ 1 ptr)))])
       (heap-set! (+ tail 3 i) (heap-ref (+ ptr 3 i)))
       )
     (heap-set! ptr 'beenheredonethat)
     (heap-set! (+ 1 ptr) tail)
     (heap-set! 3 (+ 3 die tail))
     tail]
    [(beenheredonethat) (heap-ref (+ ptr 1))]
   
    [else (error "unexpected tag @ ~a" ptr)]))

(define (header)
  (define head (heap-ref 2))
  (define tail (heap-ref 3))
  (when (< head tail)
    (case (heap-ref head)
      [(flat)
       (heap-set! 2 (+ head 2))
       (header)
       ]
      [(cons)
       (define f1 (heap-ref (+ 1 head)))
       (define f2 (heap-ref (+ 2 head)))
       (define thing (trailer f1))
       (define thing2 (trailer f2))
       (heap-set! (+ 1 head) thing)
       (heap-set! (+ 2 head) thing2) 
       (heap-set! 2 (+ 3  head))
       (header)
       ]
      [(clos)
       (define fu (heap-ref (+ 1 head)))
       (for ([i (in-range 0 fu)])
         (define helpme (trailer (heap-ref (+ head 3 i))))
         (heap-set! (+ 3 i  head) helpme ))
       
       (heap-set! 2 (+ 3 fu head))
       (header)
     
       ]
    
    
   
      )))
;;unnecessary
(define (validate-heap)
  (validate-heap-loop 0))
(define (validate-heap-loop i)
  (when (< i (heap-size))
    (case (heap-ref i)
      [(flat)
       (unless (heap-value? (heap-ref (+ i 1)))
         (error 'validate-heap "unexpected heap value @ ~a" i))
       (validate-heap-loop (+ i 2))]
      [(cons)
       (validate-pointer (+ i 1))
       (validate-pointer (+ i 2))
       (validate-heap-loop (+ i 3))]
      [(clos)
       (for ([j (in-range (heap-ref (+ i 2)))])
         (validate-pointer (+ i 3 j)))
       (validate-heap-loop (+ i 3 (heap-ref (+ i 2))))]
      [(free)
       (validate-heap-loop (+ i 1))]
      [else
       (error 'validate-heap "unexpected tag @ ~a" i)])))

(define (validate-pointer i)
  (define ptr (heap-ref i))
  (unless (and (integer? ptr)
               (>= ptr 0)
               (< ptr (heap-size))
               (member (heap-ref ptr) '(flat cons clos)))
    (error 'validate-pointer "bad pointer @ ~a" i)))

