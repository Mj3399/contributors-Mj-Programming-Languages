#lang plai/gc2/mutator
(allocator-setup "gc.rkt" 200)
(define (build-one) (let* ((x0 'y) (x1 #t)) x1))
(define (traverse-one x1) (let ((res x1)) (if (boolean? res) res #f)))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)