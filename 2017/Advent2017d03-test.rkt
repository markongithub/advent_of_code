#lang racket/base
 
(require rackunit
         "Advent2017d03.rkt")

(check-equal? (takeStep 0 0 0) (cons -1 0) "One step left")
(check-equal? (spiral 1) (list (cons 0 0)) "this is nonsense")
(check-equal? (spiral 4) (list (cons 0 0) (cons 1 0) (cons 1 1) (cons 0 1)) "this is nonsense")


