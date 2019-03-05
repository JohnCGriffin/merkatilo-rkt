#lang racket/base

;; Convenience functions and macros for application use, not
;; consumption within library.  In particular, the in-dates macro
;; is handy but lower performance than using the more verbose
;; (in-vector (dateset-vector ...)) call.

(require (only-in racket/contract contract-out -> or/c ->*)
	 (only-in "private/contracts.rkt" series-name? S)
         "core/series.rkt"
         "load.rkt"
         "prepend.rkt"
         "core/dates.rkt")

(provide
 lo-set-dates
 (contract-out
  [ in-dates (->* () (dateset?) sequence?) ]
  [ lo-or-not (-> series-name? (or/c #f S)) ]
  [ lo-prepended (-> series-name? #:with-surrogate-id series-name? S) ]))


(define (lo-set-dates id)
  (define loaded (lo id))
  (current-dates (dates loaded))
  loaded)

(define (in-dates (dates (current-dates)))
  (in-vector (dateset-vector dates)))

(define (lo-prepended id #:with-surrogate-id with-surrogate-id)
  (define primary (lo id))
  (define surrogate (lo with-surrogate-id))
  (define common-dates (dates primary surrogate #:union #t))
  (prepend primary #:with-surrogate surrogate #:dates common-dates))

(define (lo-or-not id)
  (with-handlers ([ exn:fail? not ])
    (lo id)))



;===============================================

; data no longer available for test

#;(module* test racket/base
  (require rackunit
           (submod "..")
           "private/test-support.rkt")

  (check-not-exn
   (λ ()
     (lo-or-not 'UNAVAILABLE)))

  (check-not-exn
   (λ ()
     (lo-or-not 'UNAVAILABLE::VOLUME)))

  (with-dates TEST-SERIES
    (printf "test 3\n")
    (check-equal?
     (for/sum ((i (in-dates))) 1)
     754))

  (check-equal?
   (printf "test4 \n")
   (for/sum ((i (in-dates (dates TEST-SERIES)))) 1)
   754))
