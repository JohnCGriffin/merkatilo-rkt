#lang racket/base

;; A warp simply shifts a data series N periods into the future (or past
;; when N is negative).  A typical use would be to warp signal data for
;; calculating trades on the next market day.

(require "private/common-requirements.rkt"
         (rename-in racket/unsafe/ops
                    [ unsafe-fx+ fx+ ]
                    [ unsafe-fx- fx- ]
                     [ unsafe-fx< fx< ]
                     [ unsafe-fx<= fx<= ]))

(provide (contract-out [ warp (->* (S integer?) (#:dates DS) S) ]))

(define (warp s N #:dates (dts (current-dates)))

  (define-values (dv vv fd out-v) (common-setup s dts))
  (define ld (last-date dts))
  (define dv-len (vector-length dv))

  (for ((val (in-vector vv))
        (ndx (in-naturals))
        #:when val)
    (define warp-ndx (fx+ ndx N))
    (define target-date (and (fx< -1 warp-ndx)
                             (fx< warp-ndx dv-len)
                             (vector-ref dv warp-ndx)))
    (define target-slot (and target-date
                             (fx<= fd target-date)
                             (fx<= target-date ld)
                             (fx- target-date fd)))
    (when target-slot
      (vector-set! out-v target-slot val)))

  (make-vector-series
   #:name (format "(warp ~a ~a)" (abbreviate s) N)
   #:vector out-v
   #:first-date fd))




;=================================================

(module+ main
  (require "private/test-support.rkt")
  (typical-run #:iterations 1000
   (λ () TEST-SERIES)
   (λ () (warp TEST-SERIES 2))))


;===============================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "private/test-support.rkt")

  (with-dates TEST-SERIES

    (define wa1 (warp TEST-SERIES 1))
    (define back-again (warp wa1 -1))
    ;(dump TEST-SERIES wa1 back-again)

    (check-equal?
     (add1 (series-count back-again))
     (series-count TEST-SERIES))

    (check-not-exn
     (λ ()
       (define reduced-dates
         (dates (current-dates) #:last (sub1 (last-date))))
       (with-dates reduced-dates
         (verify-equivalency back-again
                             TEST-SERIES))))))
       


