#lang racket/base

(provide figureCoords markClaims countConflicts parseClaim solvePart1File)

(require racket/file)
(require racket/match)
(require racket/set)

(define (figureCoords startX startY width height)
  (for*/list ([x (in-range startX (+ startX width))]
              [y (in-range startY (+ startY height))])
             (cons x y)
       )
  )

(define (markClaim claim h)
  (match claim
         [(list claimID startX startY width height)
          (foldl (lambda (p hh) (hash-update hh p (lambda (st) (set-add st claimID)) (set)))
                 h (figureCoords startX startY width height))]
         )
  )

(define (markClaims claims)
  (foldl markClaim (hash) claims)
  )

(define (countConflicts claims)
  (length (filter (lambda (s) (> (set-count s) 1)) (hash-values (markClaims claims))))
  )

(define (parseClaim s)
  (map string->number
       (cdr
         (regexp-match #px"^.([\\d]+) @ ([\\d]+),([\\d]+): ([\\d]+)x([\\d]+)" s)
         )
       )
  )

(define (solvePart1File f)
  (countConflicts (map parseClaim (file->lines f)))
  )
