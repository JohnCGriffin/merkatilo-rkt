#lang racket/base

;; A fractional moving average takes some fraction (< 0 Fraction 1) as
;; the weight of the current element and (1-fraction) as the weight of
;; the previous element.  Thus, a 9/10 fractional would take 9/10 of the
;; current value plus 1/10 of the previously calculated fractional.
;;
;; An EMA is a commonly used derivation with its fraction calculated as
;; (/ 2 (+ N 1)).  Thus, an EMA of N=5 is 2/(5+1) or 1/3.  An EMA of N=1
;; is simply an identity of the original series.
;;
;; The first observation and any observation following a missing observation
;; is simply the current value (weight=1).  Thus, the output has as many
;; observations as the source series.


(require
 (combine-in
  "private/common-requirements.rkt"
  (only-in "private/contracts.rkt" periodic/c)))

(provide
 (contract-out
  [ ema periodic/c ]))


(define (fractional s _fraction #:dates (dts (current-dates)))

  (define-values (dv vv fd out-v) (common-setup s dts))
  (define fraction (* 1.0 _fraction))
  (define prev-fraction (- 1.0 fraction))

  (for/fold ((prev #f))
            ((dt (in-vector dv))
             (val (in-vector vv)))
    (let ((next (if (and val prev)
                    (+ (* fraction val)
                       (* prev-fraction prev))
                    val)))
      (when val
        (vector-set! out-v (- dt fd) next))
      next))

  (make-vector-series
   #:first-date fd
   #:name (format "(fractional ~a ~a)" (abbreviate s) fraction)
   #:vector out-v))



(define (ema s N #:dates (dts (current-dates)))
  (define fraction (/ 2 (+ N 1)))
  (rename-series
   (fractional s fraction #:dates dts)
   (format "(ema ~a ~a)" (abbreviate s) N)))






;==================================================

(module+ main 
  (require "private/test-support.rkt")
  (typical-run (λ () (ema TEST-SERIES 30))))




;==================================================

(module+ test
  (require rackunit
           racket/vector
           "series-binop.rkt"
           "warp.rkt"
           "private/series-dates-values.rkt"
           "private/test-support.rkt")

  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES
       (verify-equivalency
        (ema TEST-SERIES 3)
        (fractional TEST-SERIES 1/2)))))

  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES
       (verify-equivalency (ema TEST-SERIES 3) EMA-3-SERIES))))

  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES
       (define ema3f (series-function (ema TEST-SERIES 3)))
       (define vals (series-dates-values TEST-SERIES (dateset-vector (current-dates))))
       (define final-value
         (for/fold ((acc (vector-ref vals 0)))
                   ((val (in-vector vals)))
           (/ (+ acc val) 2.0)))
       (check-equal? final-value (ema3f (last-date)))))))






