#lang racket/base

(provide processFile solvePart1 solvePart2)

(require racket/file)
(require racket/function)
(require racket/list)
(require racket/match)
(require racket/stream)

;; let's hash from guards to times slept in a given minute
;; a hash from guard to minute to count
(struct guardState (onDuty lastFellAsleep history) #:inspector #f)

(define (markMinuteSlept guardID h minute)
  ;; (hash-update h (car l) add1 0)
  (define (innerUpdate ih)
    (hash-update ih minute add1 0)
    )
  (hash-update h guardID innerUpdate (hash))
  )

(define (processAwakening state minute)
  (match state
         [(guardState onDuty lastFellAsleep history)
          (let ([newHistory (stream-fold (curry markMinuteSlept onDuty) history
                                         (in-range lastFellAsleep minute))])
            (guardState onDuty lastFellAsleep newHistory)
            )
          ]
         )
  )

(define (processLine str state)
  (let ([guardMatch (regexp-match #px"\\] Guard #([\\d]+)" str)]
        [wakeUpMatch (regexp-match #px":([\\d]+)\\] wakes up" str)]
        [sleepMatch (regexp-match #px":([\\d]+)\\] falls asleep" str)])
    (match
      state
      [(guardState onDuty lastFellAsleep history)
       (cond (guardMatch
               (guardState (string->number (cadr guardMatch))
                           lastFellAsleep history))
             (sleepMatch
               (guardState onDuty (string->number (cadr sleepMatch)) history))
             (wakeUpMatch
               (processAwakening state (string->number (cadr wakeUpMatch))))
             )
       ]
      )
    )
  )

(define (processLines l)
  (foldl processLine (guardState 1 0 (hash)) l)
  )

(define (processFile f)
  (processLines (sort (file->lines f) string<?))
  )

(define (sleepiestGuard state)
  (define (sumPairHash p)
    ;; cdr p is a hash of minutes to counts
    ;; hash-values cdr p is a list of counts
    (apply + (hash-values (cdr p)))
    )
  (car (argmax sumPairHash (hash->list (guardState-history state))))
  )

(define (sleepiestMinute state guardID)
  (car (argmax cdr (hash->list (hash-ref (guardState-history state) guardID))))
  )

(define (solvePart1 state)
  (* (sleepiestGuard state) (sleepiestMinute state (sleepiestGuard state)))
  )

(define (solvePart2 state)
  ;; this sucks and I am sorry. I had a hash of hashes and no type signatures
  ;; and things got really confusing. Use a language with explicit types.
  (define (addGuardIDToMinutePair guardID p)
    (cons guardID p)
    )
  (define (minuteHashToTriplets p)
    ;; takes a pair matching a guardID to a hash from minutes to counts
    ;; returns a list of triplets
    (map (curry addGuardIDToMinutePair (car p)) (hash->list (cdr p)))
    )
  (define allTriplets
    (append (map minuteHashToTriplets (hash->list (guardState-history state))))
    )
  (define sleepiestGuardMinute
    (argmax cddr 
            (apply append (map minuteHashToTriplets
                               (hash->list (guardState-history state)))))
    )
  (* (car sleepiestGuardMinute) (cadr sleepiestGuardMinute))
  )

