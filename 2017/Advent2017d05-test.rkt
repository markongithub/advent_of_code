#lang racket/base
 
(require rackunit "Advent2017d05.rkt")

(define testState (jumpState 0 (listToHash '(0 3 0 1 -3))))
(check-equal? (jumpState 0 (listToHash '(1 3 0 1 -3))) (takeStep1 testState))
(check-equal? (jumpState 0 (listToHash '(1 3 0 1 -3)))
              (iterate takeStep1 testState 1))
(check-equal? (jumpState 1 (listToHash '(2 3 0 1 -3)))
              (iterate takeStep1 testState 2))
(check-equal? (jumpState 4 (listToHash '(2 4 0 1 -3)))
              (iterate takeStep1 testState 3))
(check-equal? (jumpState 1 (listToHash '(2 4 0 1 -2)))
              (iterate takeStep1 testState 4))
(check-equal? (jumpState 5 (listToHash '(2 5 0 1 -2)))
              (iterate takeStep1 testState 5))
(check-equal? 5 (countStepsToEscape takeStep1 testState))
(check-equal? 5 (countStepsToEscape
                  takeStep1 (stateFromFile "input/d5test01.txt")))
(define (solveProblem1) (countStepsToEscape
                          takeStep1 (stateFromFile "input/d5p1.txt")))
(check-equal? 396086 (solveProblem1))
(check-equal? 10 (countStepsToEscape takeStep2 testState))
(define (solveProblem2) (countStepsToEscape
                          takeStep2 (stateFromFile "input/d5p1.txt")))
(check-equal? 28675390 (solveProblem2))
