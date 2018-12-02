#lang racket/base

(provide charCount hasTwoAndThree part1Checksum part1ChecksumFile commonLetters
         part2File)

(require racket/file)
(require racket/match)
(require "Helpers.rkt")

(define (charCount0 l h)
  (if (null? l) h
    (charCount0 (cdr l) (hash-update h (car l) add1 0))
    )
  )

(define (charCount s)
  (charCount0 (string->list s) (hash))
  )

(define (hasTwoAndThree0 l hasTwoAlready hasThreeAlready)
  (if (and hasTwoAlready hasThreeAlready) (cons #t #t)
    (if (null? l) (cons hasTwoAlready hasThreeAlready)
      (case (car l)
        [(2) (hasTwoAndThree0 (cdr l) #t hasThreeAlready)]
        [(3) (hasTwoAndThree0 (cdr l) hasTwoAlready #t)]
        [else (hasTwoAndThree0 (cdr l) hasTwoAlready hasThreeAlready)]
        )
      )
    )
  )

(define (hasTwoAndThree s)
  ;; go through the hash and determine if we have a two and a three
  (hasTwoAndThree0 (hash-values (charCount s)) #f #f)
  )

(define (countTwosAndThrees0 l twos threes)
  (if (null? l) (cons twos threes)
    (match (hasTwoAndThree (car l))
           [(cons hasTwo hasThree)
            (countTwosAndThrees0 (cdr l) (if hasTwo (add1 twos) twos)
                                 (if hasThree (add1 threes) threes))
            ]
           )
    )
  )

(define (countTwosAndThrees l) (countTwosAndThrees0 l 0 0))

(define (part1Checksum l)
  (match (countTwosAndThrees l)
         [(cons twos threes) (* twos threes)]
         )
  )

(define (part1ChecksumFile f)
  (part1Checksum (file->lines f))
  )

(define (listsDifferByExactlyOne l1 l2)
  (if (or (null? l1) (null? l2)) #f
    (if (equal? (car l1) (car l2)) (listsDifferByExactlyOne (cdr l1) (cdr l2))
      ;; we have found their single difference. their cdrs must be equal.
      (equal? (cdr l1) (cdr l2))
      )
    )
  )

(define (allPairs l)
  (if (null? l) (list)
    (let ([pairsWithCar (map (lambda (x) (cons (car l) x)) (cdr l))]
          [pairsWithoutCar (allPairs (cdr l))])
      (append pairsWithCar pairsWithoutCar)
      )
    )
  )

(define (findCorrectBoxes ids)
  (filterForOne (allPairs ids)
                (lambda (p) (listsDifferByExactlyOne (car p) (cdr p))))
  )

(define (idCommonChars l1 l2)
  (if (equal? (car l1) (car l2))
    (cons (car l1) (idCommonChars (cdr l1) (cdr l2)))
    ;; we know there is only one differing character, so if the cars don't match
    ;; we are sure the cdrs are good
    (cdr l1)
    )
  )

(define (commonLetters boxIDs)
  (match (findCorrectBoxes (map string->list boxIDs))
         [(cons l1 l2) (list->string (idCommonChars l1 l2))]
         )
  )

(define (part2File f)
  (commonLetters (file->lines f))
  )
