#lang racket/base

(require "private/common-requirements.rkt")

(provide
 (contract-out
  [ first-ob (->* (S) (#:dates DS) OB) ]
  [ last-ob  (->* (S) (#:dates DS) OB) ]
  [ first-value (->* (S) (#:dates DS) (or/c #f real?)) ]
  [ last-value  (->* (S) (#:dates DS) (or/c #f real?)) ]))



(define (first-ob s #:dates (dts (current-dates)))
  (define sf (series-function s))
  (for/first ((dt (in-vector (dateset-vector dts)))
              #:when (sf dt))
    (observation dt (sf dt))))

(define (first-value s #:dates (dts (current-dates)))
  (define ob (first-ob s #:dates dts))
  (and ob (observation-value ob)))


(define (last-ob s #:dates (dts (current-dates)))
  (define sf (series-function s))
  (define dv (dateset-vector dts))
  (for/first ((dt (in-vector dv (- (vector-length dv) 1) -1 -1))
              #:when (sf dt))
    (observation dt (sf dt))))

(define (last-value s #:dates (dts (current-dates)))
  (define ob (last-ob s #:dates dts))
  (and ob (observation-value ob)))



;===================================================

(module+ test
  (require rackunit
           racket/vector
           "series-binop.rkt"
           "warp.rkt"
           "private/series-dates-values.rkt"
           "private/test-support.rkt")

  (check-equal?
   (ob-v (first-ob TEST-SERIES #:dates (dates TEST-SERIES)))
   (first-value TEST-SERIES #:dates (dates TEST-SERIES)))

  (check-equal?
   (ob-d (first-ob TEST-SERIES #:dates (dates TEST-SERIES)))
   (->jdate '2012-01-03))

  (check-equal?
   (ob-d (last-ob TEST-SERIES #:dates (dates TEST-SERIES)))
   (->jdate '2014-12-31)))

