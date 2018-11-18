#lang racket/base
(require racket/match)

(provide takeStep spiralState initialSV squareXY squareDistance day3Problem2 spiralOfLength)
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

(define (squareXY squareID)
  ;; The path is backwards so we always want the first one
  (car (spiralState-path (spiralOfLength squareID)))
)

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

(define (spiral0 state termPred)
  (match state
    [(spiralState x y direction leftInSegment segmentLength path maxValue squareValues totalSteps)
     (if (termPred state) state
       (match (nextVector state) [(list newDirection newLeft newLength)
         (match (takeStep x y newDirection) [(cons newX newY)
           (let ([newValue (newSquareValue newX newY squareValues)])
             (let ([newMax (max newValue maxValue)]
                   [newValues (hash-set squareValues (cons newX newY) newValue)])
               (spiral0 (spiralState newX newY newDirection newLeft newLength (cons (cons newX newY) path) newMax newValues (+ totalSteps 1)) termPred)
             )
           )
         ])
       ])
     )
    ]
  )
)

(define (spiralOfLength steps) (spiral0 initialState (lambda (state) (>= (spiralState-totalSteps state) steps))))

(define (spiralUntilMinValue minValue) (spiral0 initialState (lambda (state) (> (spiralState-maxValue state) minValue))))


(define day3Problem2 (spiralState-maxValue (spiralUntilMinValue 361527)))
