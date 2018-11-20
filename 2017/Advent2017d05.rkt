#lang racket/base

(provide listToHash jumpState takeStep1 iterate countStepsToEscape stateFromFile takeStep2)

(require racket/file)
(require racket/match)

(struct jumpState (position offsets) #:inspector #f)

(define (listToHash0 h l i)
  (if (null? l) h
    (listToHash0 (hash-set h i (car l)) (cdr l) (+ i 1))
  )
)

(define (listToHash l) (listToHash0 (hash) l 0))

(define (takeStep1 state)
  (match state
    [(jumpState position offsets)
      (let ([currentValue (hash-ref offsets position)])
        (jumpState (+ position currentValue)
                   (hash-set offsets position (+ currentValue 1))
        )
      )
    ]
  )
)

(define (iterate f state count)
  (if (= count 0) state
    (iterate f (f state) (- count 1))
  )
)

(define (countStepsUntil0 f pred state count)
  (if (pred state) count
    (countStepsUntil0 f pred (f state) (+ count 1))
  )
)

(define (countStepsUntil f pred state) (countStepsUntil0 f pred state 0))

(define (countStepsToEscape stepFunc state)
  (let ([hasEscaped (lambda (s) (>= (jumpState-position s)
                                    (hash-count (jumpState-offsets s))
                                )
                    )
        ])
    (countStepsUntil stepFunc hasEscaped state)
  )
)

(define (listIntsFromFile f)
  (map string->number (file->lines f))
)

(define (stateFromFile f)
  (jumpState 0 (listToHash (listIntsFromFile f)))
)

(define (takeStep2 state)
  (match state
    [(jumpState position offsets)
      (let ([currentValue (hash-ref offsets position)])
        (let ([newValue (if (>= currentValue 3)
                          (- currentValue 1) (+ currentValue 1))])
          (jumpState (+ position currentValue)
                     (hash-set offsets position newValue)
          )
        )
      )
    ]
  )
)


