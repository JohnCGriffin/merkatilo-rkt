#lang racket/base

(require
 (combine-in
  "private/common-requirements.rkt"
  "load.rkt"
  "momentum.rkt"
  "convenience.rkt"
  (only-in racket/contract <=/c)))

(define capture? (->* (S #:benchmark S #:period real?)
                      (#:dates dateset?)
                      real?))
(provide
 (contract-out
  [ down-capture capture? ]
  [ up-capture capture? ]))


(define (capture s
                 #:benchmark benchmark
                 #:dates dates
                 #:period period
                 #:comparator comparator)
  (define S-mo-f (series-function (mo s period #:dates dates)))
  (define B-mo-f (series-function (mo benchmark period #:dates dates)))
  (for/fold ((total-B 0)
             (total-S 0)
             #:result (and (not (zero? total-B))
                           (/ total-S total-B)))
            ((dt (in-vector (dateset-vector dates))))
    (define B-val (B-mo-f dt))
    (define S-val (and B-val
                       (comparator B-val 0)
                       (S-mo-f dt)))
    (if S-val
        (values (+ total-B B-val)
                (+ total-S S-val))
        (values total-B total-S))))

(define (up-capture s #:benchmark benchmark #:period period #:dates (dates (current-dates)) )
  (capture s #:benchmark benchmark #:dates dates #:period period #:comparator >))

(define (down-capture s #:benchmark benchmark #:period period #:dates (dates (current-dates)))
  (capture s #:benchmark benchmark #:dates dates #:period period #:comparator <))


;==================================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "warp.rkt"
           "private/test-support.rkt")

  ; regression tests
  
  (with-dates TEST-SERIES
    
    (check-equal?
     (approx (down-capture TEST-SERIES #:benchmark (warp TEST-SERIES 10) #:period 63))
     (approx 0.5898654044835749))
    
    (check-equal?
     (approx (up-capture TEST-SERIES #:benchmark (warp TEST-SERIES 10) #:period 21))
     (approx 0.4715927637890245))))
