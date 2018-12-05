#lang racket/base
 
(require rackunit "Advent2018d04.rkt")

(check-equal? 240 (solvePart1 (processFile "input/Advent2018d04-test.txt")))
(check-equal? 103720 (solvePart1 (processFile "input/Advent2018d04.txt")))
(check-equal? 4455 (solvePart2 (processFile "input/Advent2018d04-test.txt")))
(check-equal? 4455 (solvePart2 (processFile "input/Advent2018d04-test.txt")))
(check-equal? 110913 (solvePart2 (processFile "input/Advent2018d04.txt")))
