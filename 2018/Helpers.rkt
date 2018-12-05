#lang racket/base

;; functions I suspect will be used in multiple days of AOC.
(provide listIntsFromFile iterateUntil filterForOne)

(require racket/file)

(define (listIntsFromFile f)
  (map string->number (file->lines f))
  )

(define (iterateUntil f pred state)
  ;; applies f to state over and over until (pred state) becomes true.
  (if (pred state) state
    (iterateUntil f pred (f state))
    )
  )

(define (filterForOne l pred)
  (if (pred (car l)) (car l)
    (filterForOne (cdr l) pred)
    )
  )