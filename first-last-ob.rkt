#lang racket/base

(require "private/common-requirements.rkt")

(provide
 (contract-out
  [ first-ob (->* (S) (#:dates DS) OB) ]
  [ last-ob  (->* (S) (#:dates DS) OB) ]))



(define (first-ob s #:dates (dts (current-dates)))
  (define sf (series-function s))
  (or (for/first ((dt (in-vector (dateset-vector dts)))
                  #:when (sf dt))
        (observation dt (sf dt)))
      (raise-user-error 'first-ob "no observation found")))


(define (last-ob s #:dates (dts (current-dates)))
  (define sf (series-function s))
  (define dv (dateset-vector dts))
  (or (for/first ((dt (in-vector dv (- (vector-length dv) 1) -1 -1))
                  #:when (sf dt))
        (observation dt (sf dt)))
      (raise-user-error 'last-ob "no observation found")))




;===================================================

(module* test racket/base
  (require rackunit
           racket/vector
           (submod "..")
           "series-binop.rkt"
           "warp.rkt"
           "private/series-dates-values.rkt"
           "private/test-support.rkt")


  (with-dates TEST-SERIES

    (check-exn
     exn?
     (λ ()
       (last-ob NEVER-SERIES)))
    
    (check-exn
     exn?
     (λ ()
       (first-ob NEVER-SERIES)))
    
    (check-equal?
     (ob-d (first-ob TEST-SERIES))
     (->jdate '2012-01-03))

    (check-equal?
     (ob-d (last-ob TEST-SERIES))
     (->jdate '2014-12-31)))

  (check-equal?
   (ob-d (first-ob TEST-SERIES #:dates (dates TEST-SERIES)))
   (->jdate '2012-01-03))

  (check-equal?
   (ob-d (last-ob TEST-SERIES #:dates (dates TEST-SERIES)))
   (->jdate '2014-12-31)))

