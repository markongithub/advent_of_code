#lang racket/base
 
(require rackunit "Advent2018d03.rkt")

(check-equal? 4 (countConflicts (list (list 1 3 4 4)
                                      (list 3 1 4 4)
                                      (list 5 5 2 2))))
(check-equal? 4 (countConflicts (map parseClaim (list "#1 @ 1,3: 4x4"
                                                      "#2 @ 3,1: 4x4"
                                                      "#3 @ 5,5: 2x2"))))
(check-equal? 4 (solvePart1File "input/Advent2018d03.txt"))
