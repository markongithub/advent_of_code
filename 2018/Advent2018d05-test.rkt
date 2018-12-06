#lang racket/base
 
(require rackunit "Advent2018d05.rkt")

(define (testElim s) (list->string (eliminateFirstPair (string->list s))))
(check-equal? "" (allEliminations "aA"))
(check-equal? "" (allEliminations "abBA"))
(check-equal? "abAB" (allEliminations "abAB"))
(check-equal? "aabAAB" (allEliminations "aabAAB"))
(check-equal? "dabAaCBAcCcaDA" (testElim "dabAcCaCBAcCcaDA"))
(check-equal? "dabCBAcCcaDA" (testElim "dabAaCBAcCcaDA"))
(check-equal? "dabCBAcaDA" (testElim "dabCBAcCcaDA"))
(check-equal? "dabCBAcaDA" (testElim "dabCBAcaDA"))
(check-equal? 10 (solvePart1 "dabAcCaCBAcCcaDA"))
(check-equal? 9238 (solvePart1File "input/Advent2018d05.txt"))
(check-equal? 4 (solvePart2 "dabAcCaCBAcCcaDA"))
(check-equal? 4052 (solvePart2File "input/Advent2018d05.txt"))

