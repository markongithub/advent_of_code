#lang racket/base

(provide listToHash iterate iterateUntil countStepsUntil)

(require racket/file)

(define (listToHash0 h l i)
  (if (null? l) h
    (listToHash0 (hash-set h i (car l)) (cdr l) (+ i 1))
  )
)

(define (listToHash l) (listToHash0 (hash) l 0))

(define (iterate f state count)
  (if (= count 0) state
    (iterate f (f state) (- count 1))
  )
)

(define (iterateUntil f pred state)
  (if (pred state) state
    (iterateUntil f pred (f state))
  )
)

(define (countStepsUntil0 f pred state count)
  (if (pred state) count
    (countStepsUntil0 f pred (f state) (+ count 1))
  )
)

(define (countStepsUntil f pred state) (countStepsUntil0 f pred state 0))

(define (listIntsFromFile f)
  (map string->number (file->lines f))
)
