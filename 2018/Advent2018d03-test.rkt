#lang racket/base
 
(require rackunit "Advent2018d03.rkt")

(check-equal? 4 (countConflicts (list (list 1 1 3 4 4)
                                      (list 2 3 1 4 4)
                                      (list 3 5 5 2 2))))
(define testClaims (map parseClaim (list "#1 @ 1,3: 4x4"                                                                 "#2 @ 3,1: 4x4"
                                         "#3 @ 5,5: 2x2")))
(check-equal? 4 (countConflicts testClaims))
(check-equal? 120419 (solvePart1File "input/Advent2018d03.txt"))
(check-equal? 3 (claimWithoutConflicts testClaims))
(check-equal? 445 (solvePart2File "input/Advent2018d03.txt"))