#lang racket/base
 
(require rackunit "Advent2018d01.rkt")

(check-equal? 408 (solvePart1File "input/Advent2018d01.txt"))
(check-equal? 0 (solvePart2 (list 1 -1)))
(check-equal? 10 (solvePart2 (list 3 3 4 -2 -4)))
(check-equal? 5 (solvePart2 (list -6 3 8 5 -6)))
(check-equal? 14 (solvePart2 (list 7 7 -2 -7 -4)))
(check-equal? 55250 (solvePart2File "input/Advent2018d01.txt"))
