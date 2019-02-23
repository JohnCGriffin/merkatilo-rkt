#lang racket/base

;; Given an investment and a signal series, buy and sell the asset,
;; optionally moving funds into alternate-investment.  The result is the
;; valuation after making trades dictated by the +1 buys and -1 sells of
;; the signal series.  The investment is assumed to start invested,
;; awaiting a -1 signal to exit the invested position.


(require "private/common-requirements.rkt"
	 "first-last-ob.rkt")

(provide
 (contract-out
  [ equity-line (->* (S S) (#:alternate-investment (or/c #f S) #:dates DS) S)]))


(define (equity-line s signals
		     #:init (initial-value 100)
                     #:alternate-investment (alternate-investment #f)
                     #:dates (dts (current-dates)))

  (define init-value (exact->inexact initial-value))
  (define dv (dateset-vector dts))
  (define alt-v (if alternate-investment
                    (series-dates-values alternate-investment dv)
                    (make-vector (vector-length dv) 1)))
  (define inv-v (series-dates-values s dv))
  (define sig-v (series-dates-values signals dv))
  (define-values (fd out-v)
    (dates-appropriate-fd-and-vec dts))
  (define first-sig-date (ob-d (first-ob signals #:dates dts)))

  (define (oops msg dt)
    (raise-user-error 'equity-line msg (jdate->text dt)))

  (for/fold ((product 1.0)
	     (buy? #t)
	     (prev-inv #f)
	     (prev-alt #f))
            ((dt (in-vector dv))
             (alt (in-vector alt-v))
             (inv (in-vector inv-v))
             (sig (in-vector sig-v))
             #:when (and (>= dt first-sig-date)
                         (or (and inv alt) sig)))
    
    (define change
      (cond
        ((not inv) (oops "missing investment observation at ~a" dt))
        ((not alt) (oops "missing alternative investment observation at ~a" dt))
        (buy? (if prev-inv (/ inv prev-inv) 1.0))
        (else (if prev-alt (/ alt prev-alt) 1.0))))

    (let ((new-buy? (if sig (> sig 0) buy?))
	  (new-product (* product change)))
      (vector-set! out-v (- dt fd) (* new-product init-value))
      (values new-product new-buy? inv alt)))
  

  (make-vector-series
   #:name "equity-line"
   #:vector out-v
   #:first-date fd))






;=============================================

(module+ main
  
  (require "private/test-support.rkt"
           "cross.rkt"
           "ema.rkt")

  (define crossed
    (with-dates TEST-SERIES
      (cross #:slower (ema TEST-SERIES 10) #:faster TEST-SERIES)))

  (typical-run (λ () (equity-line TEST-SERIES crossed))))


;============================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "cross.rkt"
           "ema.rkt"
           "private/test-support.rkt")

  (with-dates TEST-SERIES

    (define ALT (series (λ (dt) (* 1.0 dt)) "reflector"))

    (define crossed
      (cross #:slower (ema TEST-SERIES 10) #:faster TEST-SERIES))

    ; Just to exercise code path
    (check-not-exn
     (λ ()
       (equity-line TEST-SERIES crossed #:alternate-investment ALT)))

    (check-not-exn
     (λ ()
       (verify-equivalency
        (equity-line TEST-SERIES crossed)
        EQUITYLINE-EMA-10)))))


