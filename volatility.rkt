#lang racket/base

(require
 (combine-in
  "private/common-requirements.rkt"
  "momentum.rkt"
  "private/utilities.rkt"))

(provide
 (contract-out
  [ volatility          (->* (S) (#:days integer? #:dates DS) positive-real?)]
  [ volatility-residual (->* (S) (#:days integer? #:dates DS) positive-real?)]))



; volatility here means the standard deviation
; of one-year gain/loss ratios.  Notice that it
; uses mo-days for 365 days, not 365 periods.

(define (volatility s
                    #:days (days 365)
                    #:dates (dts (current-dates)))

  (define dv (dateset-vector dts))
  (define sv (series-dates-values (mo-days s days) dv))
  (standard-deviation sv))



(define (volatility-residual s
                             #:days (days 365)
                             #:dates (dts (current-dates)))
  (- 1 (volatility s #:days days #:dates dts)))





;======================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "private/test-support.rkt")

  (with-dates TEST-SERIES
    
    (check-equal?
     (approx (volatility TEST-SERIES))
     (approx 0.038702419488645126))
    
    (check-equal?
     (approx (volatility-residual TEST-SERIES))
     (approx 0.9612975805113548))
    
    (check-equal?
     (approx (volatility TEST-SERIES #:days 200))
     (approx 0.0468086666214253))))



