#lang racket/base

(provide eliminateFirstPair allEliminations solvePart1 solvePart1File
         solvePart2 solvePart2File debugPart2)

(require racket/file)
(require racket/function)
(require racket/list)
(require racket/match)
(require racket/stream)
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

(define (eliminateOneLetter l char)
  (define (isMatch c)
    (or (char=? c char) (char=? c (char-upcase char)))
    )
  (filter (compose not isMatch) l)
  )

;; I don't know a better way to do this
(define theAlphabet (stream->list 
                      (stream-map integer->char (in-range 97 123))))


(define (solvePart1 s)
  (string-length (allEliminations s))
  )

(define (solvePart1File f)
  (solvePart1 (car (file->lines f)))
  )

(define (solvePart2 s)
  (define allPossiblePolymers (map (compose list->string (curry eliminateOneLetter (string->list s))) theAlphabet))
  (apply min (map solvePart1 allPossiblePolymers))
  )

(define (debugPart2 s)
  (define (eliminateLetter c)
    ((compose list->string (curry eliminateOneLetter (string->list s))) c)
    )
  (define allPossiblePolymers (map eliminateLetter (take theAlphabet 4)))
  (take theAlphabet 4)
  ;;  (eliminateOneLetter (string->list s) #\d)
  (map allEliminations allPossiblePolymers)
)
  
(define (solvePart2File f)
  (solvePart2 (car (file->lines f)))
  )
