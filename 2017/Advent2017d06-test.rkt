#lang racket/base
 
(require rackunit "Helpers.rkt" "Advent2017d06.rkt")

(define testBanks (listToHash '(0 2 7 0)))
(check-equal? (listToHash '(2 4 1 2)) (redistribute testBanks))
(check-equal? (listToHash '(3 1 2 3)) (iterate redistribute testBanks 2))
(check-equal? (listToHash '(0 2 3 4)) (iterate redistribute testBanks 3))
(check-equal? (listToHash '(1 3 4 1)) (iterate redistribute testBanks 4))
(check-equal? (listToHash '(2 4 1 2)) (iterate redistribute testBanks 5))
(check-equal? 5 (countStepsToDupe (stateFromList '(0 2 7 0))))
(check-equal? 7864 (countStepsToDupe (part1Input)))
(check-equal? 4 (cycleLength (stateFromList '(0 2 7 0))))
(check-equal? 1695 (cycleLength (part1Input)))
