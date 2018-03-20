#lang racket/base

;; cross takes two series, one dubbed 'faster' and the other 'slower' to
;; generate signals.  The faster series is usually a price series and the
;; slower is usually a slower-changing smoothing of the same data such as
;; an EMA result.  When the slower series crosses from below to above the
;; faster series, a +1 signal is generated.  A -1 signal occurs above
;; crossing below.

;; The border can be adjusted with upside-factor and downside-factor that
;; default to 1.  For instance, an upside-factor of 1.02 requires the
;; slower series to cross 2% higher than the faster series.


(require "private/common-requirements.rkt")


(provide
 (contract-out
  [ cross (->* (#:slower S #:faster S)
               (#:upside-factor   (between/c .8 1.5)
		#:downside-factor (between/c .5 1.2)
		#:dates DS)
               S)]))


(define (cross #:slower slower
               #:faster faster
               #:upside-factor (upside-factor 1.0)
               #:downside-factor (downside-factor 1.0)
               #:dates (dts (current-dates)))

  (define dv (dateset-vector dts))
  (define sv (series-dates-values slower dv))
  (define fv (series-dates-values faster dv))
  (define-values (fd out-v)
    (dates-appropriate-fd-and-vec dts))

  (for/fold ((prev-sig #f))
      ((dt (in-vector dv))
       (slow (in-vector sv))
       (fast (in-vector fv))
       #:when (and slow fast))

    (define sig
      (cond
       ((> fast (* upside-factor slow)) 1.0)
       ((< fast (* downside-factor slow)) -1.0)
       (else #f)))

    (if (and sig
	     (not (eqv? sig prev-sig)))
	(and
	 (vector-set! out-v (- dt fd) sig)
	 sig)
	prev-sig))
  
  (make-vector-series
   #:name (format "(cross #:faster ~a #:slower ~a)" (abbreviate faster) (abbreviate slower))
   #:vector out-v
   #:first-date fd))




 ;=============================================================

(module+ main
  (require "private/test-support.rkt"
           "ma.rkt")

  (with-dates TEST-SERIES
	      (define M3 (ma TEST-SERIES 3))
	      (typical-run
	       (λ () TEST-SERIES)
	       (λ () M3)
	       (λ () (cross #:faster TEST-SERIES #:slower M3)))))



(module* test racket/base
  (require rackunit
           (submod "..")
           "ema.rkt"
           "private/test-support.rkt")

  (with-dates TEST-SERIES
	      (check-not-exn
	       (λ ()
		 (verify-equivalency
		  (cross #:slower (ema TEST-SERIES 30) #:faster TEST-SERIES)
		  CROSS-EMA-30-SERIES)))))

