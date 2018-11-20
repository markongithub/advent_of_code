#lang racket/base
 
(require rackunit "Advent2017d04.rkt")

(check-equal? #f (listContainsDupe (list "no" "dupes" "here")))
(check-equal? #t (listContainsDupe (list "dupe" "dee" "dupe")))
(check-equal? #t (validatePassphrase1 "no dupes here"))
(check-equal? #f (validatePassphrase1 "dupe dee dupe"))
(check-equal? #f (validatePassphrase1 "dupe dee dupe"))
(check-equal? 2 (countValidFromFile validatePassphrase1 "input/d4test01.txt"))
(check-equal? 451 (countValidFromFile validatePassphrase1 "input/d4p1.txt"))
(check-equal? #t (validatePassphrase2 "abcde fghij"))
(check-equal? #f (validatePassphrase2 "abcde xyz ecdab"))
(check-equal? #t (validatePassphrase2 "a ab abc abd abf abj"))
(check-equal? #t (validatePassphrase2 "iiii oiii ooii oooi oooo"))
(check-equal? #f (validatePassphrase2 "oiii ioii iioi iiio"))
(check-equal? 3 (countValidFromFile validatePassphrase2 "input/d4test02.txt"))
(check-equal? 3 (countValidFromFile validatePassphrase2 "input/d4test02.txt"))
(check-equal? 223 (countValidFromFile validatePassphrase2 "input/d4p1.txt"))
