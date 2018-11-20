#lang racket/base

(provide listContainsDupe validatePassphrase1 countValidFromFile validatePassphrase2)

(require racket/file)
(require racket/set)
(require racket/string)

(define (listContainsDupe0 st l)
  (if (null? l) #f
    (if (set-member? st (car l)) #t
      (listContainsDupe0 (set-add st (car l)) (cdr l))
    )
  )
)

(define (listContainsDupe l) (listContainsDupe0 (set) l))

(define (validatePassphrase1 s) (not (listContainsDupe (string-split s))))

(define (countValidPassphrases validator l) (length (filter validator l)))

(define (countValidFromFile validator f)
  (countValidPassphrases validator (file->lines f)))

(define (alphabetize s) (sort (string->list s) char<?))

(define (validatePassphrase2 s) (not (listContainsDupe
                                       (map alphabetize (string-split s)))))
