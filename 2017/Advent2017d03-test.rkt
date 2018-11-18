#lang racket/base
 
(require rackunit
         "Advent2017d03.rkt")

(check-equal? (takeStep 0 0 0) (cons -1 0) "One step left")
(check-equal? (cons 2 -2) (squareXY 25) "squareXY 25")
(check-equal? 4 (squareDistance 25) "now the distance")
(check-equal? 3 (squareDistance 12) "distance 12")
(check-equal? 2 (squareDistance 23) "distance 23")
(check-equal? 31 (squareDistance 1024) "distance 1024")
(check-equal? 326 (squareDistance 361527) "distance 361527")
(check-equal? (spiralOfLength 1) (spiralState 0 0 2 1 1 (list (cons 0 0)) 0 initialSV 1) "this is nonsense")
(check-equal? day3Problem2 363010 "answer part 2")
