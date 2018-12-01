#lang racket/base

(provide solvePart1File)

(require "Helpers.rkt")

;; string->number handles the leading + automatically!
(define (solvePart1File f)
  (apply + (listIntsFromFile f))
  )
