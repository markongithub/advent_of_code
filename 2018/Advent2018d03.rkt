#lang racket/base

(provide countConflicts parseClaim solvePart1File claimWithoutConflicts
         solvePart2File)

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
  (define (addToSet claimID)
    (lambda (s) (set-add s claimID))
    )
  (define (updateHash claimID)
    (lambda (hh coords)
      (hash-update coords hh (addToSet claimID) (set))
    )
    )
  (match claim
         [(list claimID startX startY width height)
          (foldl (updateHash claimID) h
                 (figureCoords startX startY width height))]
         )
  )

(define (markClaims claims)
  (foldl markClaim (hash) claims)
  )

(define (countConflicts claims)
  (length (filter (lambda (s) (> (set-count s) 1))
                  (hash-values (markClaims claims))))
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

(define (claimWithoutConflicts claims)
  (define (foldableSetRemove v st) (set-remove st v))
  (set-first
    (for/fold ([cleanClaims (list->set (map car claims))])
              ([claimsBySquare (hash-values (markClaims claims))])
              (if (< (set-count claimsBySquare) 2) cleanClaims
                (foldl foldableSetRemove cleanClaims (set->list claimsBySquare))
                )
              )
    )
  )

(define (solvePart2File f)
  (claimWithoutConflicts (map parseClaim (file->lines f)))
  )