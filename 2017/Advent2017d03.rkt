#lang racket/base

(provide takeStep nextSegment spiral0 spiral)
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

(define (nextSegment lastLength lastDirection)
   (cond ((= lastDirection 0) (cons lastLength 1))
         ((= lastDirection 1) (cons (+ lastLength 1) 2))
         ((= lastDirection 2) (cons lastLength 3))
         ((= lastDirection 3) (cons (+ lastLength 1) 0))
         (else (raise "invalid direction"))
   )
)

(define (spiral0 x y direction leftInSegment segmentLength totalLeft)
  (if (= totalLeft 1) (list (cons x y))
    ;;; ok we need to take a step but first we need to know if it's time
    ;;; to change direction
    (let ([newSegment
      (if (> leftInSegment 0) (list direction (- leftInSegment 1) segmentLength)
        (let ([nextSegResult (nextSegment segmentLength direction)])
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
