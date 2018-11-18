#lang racket/base
(require racket/match)

(provide takeStep turnLeft spiralState initialSV initialState spiral0 spiral squareXY squareDistance spiralA spiralB nextVector spiralState-maxValue day3Problem2)
;;; okay so the spiral0 goes
;;; 1 right, 1 up
;;; 2 left, 2 down
;;; 3 right 3 up
;;; 4 left 4 down
;;; 0=left, 1=down, 2=right, 3=up
(define (takeStep x y direction)
  (cond ((= direction 0) (cons (- x 1) y))
        ((= direction 1) (cons x (- y 1)))
        ((= direction 2) (cons (+ x 1) y))
        ((= direction 3) (cons x (+ y 1)))
        (else (raise "invalid direction"))
  )
)

(define (turnLeft lastLength lastDirection)
   (cond ((= lastDirection 0) (cons lastLength 1))
         ((= lastDirection 1) (cons (+ lastLength 1) 2))
         ((= lastDirection 2) (cons lastLength 3))
         ((= lastDirection 3) (cons (+ lastLength 1) 0))
         (else (raise "invalid direction"))
   )
)

(struct spiralState (x y direction leftInSegment segmentLength path maxValue squareValues totalSteps) #:inspector #f)

(define initialSV (hash (cons 0 0) 1))

(define initialState (spiralState 0 0 2 1 1 (list (cons 0 0)) 0 initialSV 1))

(define (spiral0 x y direction leftInSegment segmentLength totalLeft)
  (if (= totalLeft 1) (list (cons x y))
    ;;; ok we need to take a step but first we need to know if it's time
    ;;; to change direction
    (let ([newSegment
      ;;; a list of direction, leftInSegment, segmentLength
      (if (> leftInSegment 0) (list direction (- leftInSegment 1) segmentLength)
        (let ([nextSegResult (turnLeft segmentLength direction)])
          (let ([nextSegLength (car nextSegResult)])
            (list (cdr nextSegResult) (- nextSegLength 1) nextSegLength)
          )
        )
     )]) (let ([nextCoords (takeStep x y (car newSegment))])
       (cons (cons x y) (spiral0 (car nextCoords) (cdr nextCoords) (car newSegment) (car (cdr newSegment)) (car (cdr (cdr newSegment))) (- totalLeft 1) ))
      )
   )
  )
)

(define (spiral totalLength) (spiral0 0 0 2 1 1 totalLength))

(define (squareXY squareID) (list-ref (spiral squareID) (- squareID 1)))

(define (squareDistance squareID)
  (let ([coords (squareXY squareID)])
    (+ (abs (car coords)) (abs (cdr coords)))
  )
)

(define (nextVector state)
  (match state
    [(spiralState _ _ direction leftInSegment segmentLength _ _ _ _)
     (if (> leftInSegment 0) (list direction (- leftInSegment 1) segmentLength)
       (match (turnLeft segmentLength direction)
         [(cons newLength newDirection) (list newDirection (- newLength 1) newLength)]
       )
     )
    ]
  )
)

(define (neighbors x y)
  (list (cons (- x 1) (+ y 1))
        (cons x (+ y 1))
        (cons (+ x 1) (+ y 1))
        (cons (- x 1) y)
        (cons (+ x 1) y)
        (cons (- x 1) (- y 1))
        (cons x (- y 1))
        (cons (+ x 1) (- y 1))
  )
)

(define (neighborValues x y squareValues)
  (let ([lookup (lambda (xy) (hash-ref squareValues xy 0))])
    (map lookup (neighbors x y))
  )
)

(define (newSquareValue x y squareValues)
  (apply + (neighborValues x y squareValues))
)

(define (spiralA0 state totalLeft)
  (if (= totalLeft 0) state
    (match state
      [(spiralState x y direction leftInSegment segmentLength path maxValue squareValues totalSteps)
       (match (nextVector state) [(list newDirection newLeft newLength)
         (match (takeStep x y newDirection) [(cons newX newY)
           (let ([newValue (newSquareValue newX newY squareValues)])
             (let ([newMax (max newValue maxValue)]
                   [newValues (hash-set squareValues (cons newX newY) newValue)])
               (spiralA0 (spiralState newX newY newDirection newLeft newLength (cons (cons newX newY) path) newMax newValues (+ totalSteps 1)) (- totalLeft 1))
             )
           )
         ])
       ])
      ]
    )
  )
)

(define (spiralA totalLeft) (spiralA0 initialState (- totalLeft 1)))

(define (spiralB0 state minValue)
  (match state
    [(spiralState x y direction leftInSegment segmentLength path maxValue squareValues totalSteps)
     (if (> maxValue minValue) state
       (match (nextVector state) [(list newDirection newLeft newLength)
         (match (takeStep x y newDirection) [(cons newX newY)
           (let ([newValue (newSquareValue newX newY squareValues)])
             (let ([newMax (max newValue maxValue)]
                   [newValues (hash-set squareValues (cons newX newY) newValue)])
               (spiralB0 (spiralState newX newY newDirection newLeft newLength (cons (cons newX newY) path) newMax newValues (+ totalSteps 1)) minValue)
             )
           )
         ])
       ])
     )
    ]
  )
)

(define (spiralB minValue) (spiralB0 initialState minValue))


(define (spiralC0 state termPred)
  (match state
    [(spiralState x y direction leftInSegment segmentLength path maxValue squareValues totalSteps)
     (if (termPred state) state
       (match (nextVector state) [(list newDirection newLeft newLength)
         (match (takeStep x y newDirection) [(cons newX newY)
           (let ([newValue (newSquareValue newX newY squareValues)])
             (let ([newMax (max newValue maxValue)]
                   [newValues (hash-set squareValues (cons newX newY) newValue)])
               (spiralC0 (spiralState newX newY newDirection newLeft newLength (cons (cons newX newY) path) newMax newValues (+ totalSteps 1)) termPred)
             )
           )
         ])
       ])
     )
    ]
  )
)

(define (spiralUntilMinValue minValue) (spiralC0 initialState (lambda (state) (> (spiralState-maxValue state) minValue))))


(define day3Problem2 (spiralState-maxValue (spiralUntilMinValue 361527)))
