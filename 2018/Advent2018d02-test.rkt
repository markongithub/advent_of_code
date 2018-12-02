#lang racket/base
 
(require rackunit "Advent2018d02.rkt")

(check-equal? 3 (hash-ref (charCount "aaabb") #\a))
(check-equal? 2 (hash-ref (charCount "aaabb") #\b))
(check-equal? (cons #f #t) (hasTwoAndThree "aaabbb"))
(check-equal? (cons #t #t) (hasTwoAndThree "aaabb"))
(check-equal? 12 (part1Checksum (list "abcdef" "bababc" "abbcde" "abcccd"
                                      "aabcdd" "abcdee" "ababab")))
(check-equal? 3952 (part1ChecksumFile "input/Advent2018d02.txt"))
(check-equal? "fgij" (commonLetters (list "abcde" "fghij" "klmno" "pqrst"
                                          "fguij" "axcye" "wvxyz")))
(check-equal? "vtnikorkulbfejvyznqgdxpaw" (part2File "input/Advent2018d02.txt"))
