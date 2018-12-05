#lang racket/base

(provide eliminateFirstPair allEliminations solvePart1 solvePart1File) ;; solvePart2)

(require racket/file)
(require racket/function)
(require racket/list)
(require racket/match)
(require "Helpers.rkt")

(define (isMatch c1 c2)
  (and (not (char=? c1 c2))
       (or (char=? c1 (char-downcase c2))
           (char=? c1 (char-upcase c2))
           )
       )
  )

(define (eliminateFirstPair l)
  ;; eh, this will be prettier without tail recursion
  (match l [(list) l]
           [(list c) l]
           [(cons c1 (cons c2 xs))
            (if (isMatch c1 c2) xs (cons c1 (eliminateFirstPair (cdr l))))]
           )
  )

(define (allEliminations s)
  (list->string (iterateUntilStable eliminateFirstPair (string->list s)))
  )

(define (solvePart1 s)
  (string-length (allEliminations s))
  )

(define (solvePart1File f)
  (solvePart1 (car (file->lines f)))
  )
