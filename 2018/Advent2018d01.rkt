#lang racket/base

(provide solvePart1File solvePart2 solvePart2File)

(require racket/match)
(require racket/set)
(require "Helpers.rkt")

;; string->number handles the leading + automatically!
(define (solvePart1File f)
  (apply + (listIntsFromFile f))
  )

;; part 2 would be a lot easier if we could lazily evaluate infinitely long
;; lists, but we're not in Haskell anymore. I tried to figure out how to use
;; the lazy module but instead I'm going to do an outer and inner thing. in a
;; procedural language we'd just "break" when we finished but we'll be using a
;; "fold" which doesn't really break. though I think I did write a foldWithBreak
;; in Haskell a while back.
(struct part2State (currentFreq seenFreqs answerFound))

(define initialState (part2State 0 (set) #f))

(define (takeStep change state)
  ;; this is the inner function that gets called by applySteps. since foldl
  ;; doesn't know if we've found our answer yet, this might get called many
  ;; times after we've found the answer. but if answerFound is true we will just
  ;; do nothing on those calls.
  (match state
         [(part2State currentFreq seenFreqs answerFound)
          (if answerFound
            ;; we already finished, do nothing for now
            state
            (if (set-member? seenFreqs currentFreq)
              ;; we found it, mark answerFound true
              (part2State currentFreq seenFreqs #t)
              (part2State (+ change currentFreq) (set-add seenFreqs currentFreq)
                          #f)
              )
            )
          ]
         )
  )

(define (applySteps changes)
  ;; returns a function that takes a state and returns another
  ;; this is the outer routine in which we apply the same list of changes over
  ;; and over via iterateUntil
  (lambda (state) (foldl takeStep state changes))
  )

(define (solvePart2 changes)
  (let ([finalState (iterateUntil (applySteps changes) part2State-answerFound
                                  initialState)])
    (part2State-currentFreq finalState)
    )
  )

(define (solvePart2File f)
  (solvePart2 (listIntsFromFile f))
  )
