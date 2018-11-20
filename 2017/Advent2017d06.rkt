#lang racket/base

(provide redistribute stateFromList countStepsToDupe part1Input)

(require racket/file)
(require racket/list)
(require racket/match)
(require racket/set)
(require racket/string)
(require "Helpers.rkt")

(define (incrementOrLoop i numBanks)
  (modulo (+ i 1) numBanks)
)

(define (redistribute0 banks blocks pos)
  (if (= blocks 0) banks
    (let ([newPos (incrementOrLoop pos (hash-count banks))]
          [newBanks (hash-update banks pos add1)])
      (redistribute0 newBanks (- blocks 1) newPos)
    )
  )
)

(define (maxValueKey0 h maxValue maxKey pos)
  (if (>= pos (hash-count h)) maxKey
    (if (> (hash-ref h pos) maxValue)
      (maxValueKey0 h (hash-ref h pos) pos (+ pos 1))
      (maxValueKey0 h maxValue maxKey (+ pos 1))
    )
  )
)

(define (maxValueKey h)
  (maxValueKey0 h (hash-ref h 0) 0 0)
)

(define (redistribute banks)
  (redistribute0 (hash-set banks (maxValueKey banks) 0)
                 (hash-ref banks (maxValueKey banks))
                 (incrementOrLoop (maxValueKey banks) (hash-count banks))
  )
)

(struct day6State (banks past))

(define (logThenRedistribute state)
  (match state
         [(day6State oldBanks past)
          (day6State (redistribute oldBanks) (set-add past oldBanks))]
  )
)

(define (banksSeenBefore state)
  (match state
         [(day6State banks past) (set-member? past banks)]
  )
)

(define (stateFromList l) (day6State (listToHash l) (set)))

(define (countStepsToDupe state)
  (countStepsUntil logThenRedistribute banksSeenBefore state))

(define (part1Input)
  ;; why is this space delimited?!
  (stateFromList (map string->number (string-split "0    5       10      0       11      14      13      4       11      8       8       7       1       4       12      11")))
)