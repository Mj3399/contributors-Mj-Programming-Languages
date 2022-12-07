#lang racket
(require plai/random-mutator)
(save-random-mutator "more mutate.rkt" "gc.rkt" #:gc2? #t)