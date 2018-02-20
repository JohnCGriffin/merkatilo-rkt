#lang racket/base

(require
 (combine-in
  "private/common-requirements.rkt"
  "private/utilities.rkt"))

(provide
 (contract-out
  [ to-signals (->* (S) (#:dates DS) S) ]))


; A signal series contains -1 and 1 based upon negative
; and non-negative inputs.  Repeated values are excised.

(define (to-signals s #:dates (dts (current-dates)))

  (define dv (dateset-vector dts))
  (define vv (signalify-vector-copy (series-dates-values s dv)))
  (define-values (fd out-v)
    (dates-appropriate-fd-and-vec dts))

  (for ((dt (in-vector dv))
        (val (in-vector vv))
        #:when val)
    (vector-set! out-v (- dt fd) val))

  (make-vector-series
   #:name (format "(to-signals ~a)" (abbreviate s))
   #:vector out-v
   #:first-date fd))




;=================================================


(module+ main
  (require "private/test-support.rkt"
           "series-count.rkt"
           "momentum.rkt")
  (define MO20 (with-dates TEST-SERIES (mo TEST-SERIES 20)))
  (typical-run (λ () MO20)
               (λ () (to-signals MO20))))


;=================================================

(module* test racket/base
  (require rackunit
           racket/vector
           racket/string
           (submod "..")
           "obs-series.rkt"
           "private/test-support.rkt"
           "momentum.rkt")

  (define (ob->text ob)
    (format "~a:~a"
            (jdate->text (ob-d ob))
            (ob-v ob)))

  (check-equal?
   (with-dates TEST-SERIES
     (define obs (series->obs (current-dates) (to-signals (mo TEST-SERIES 240))))
     (string-join (map ob->text obs)))
   "2012-12-17:1 2013-08-30:-1 2013-09-03:1 2014-12-12:-1 2014-12-19:1"))

