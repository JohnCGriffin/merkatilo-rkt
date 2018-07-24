#lang racket/base

;; A warp simply shifts a data series N periods into the future (or past
;; when N is negative).  A typical use would be to warp signal data for
;; calculating trades on the next market day.

(require "private/common-requirements.rkt"
         (rename-in racket/unsafe/ops
                    [ unsafe-vector-set! vector-set! ]
                    [ unsafe-fx+ fx+ ]
                    [ unsafe-fx- fx- ]
                    [ unsafe-fx< fx< ]
                    [ unsafe-fx<= fx<= ]))

(provide (contract-out [ warp (->* (S integer?) (#:dates DS) S) ]))

(define (warp s N #:dates (dts (current-dates)))

  (define-values (dv vv fd out-v) (common-setup s dts))
  (define ld (last-date dts))
  (define dv-len (vector-length dv))

  (define-values (dv-start vv-start)
    (cond
      ((>= (abs N) dv-len)
       (values dv-len dv-len))
      ((< N 0)
       (values 0 (abs N)))
      (else
       (values N 0))))

  (for ((val (in-vector vv vv-start ))
        (dt (in-vector dv dv-start))
        #:when val)
    (vector-set! out-v (- dt fd) val))

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
       


