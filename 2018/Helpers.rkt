#lang racket/base

(provide listIntsFromFile)

(require racket/file)

(define (listIntsFromFile f)
  (map string->number (file->lines f))
)
