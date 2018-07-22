#lang racket/base

(require "private/common-requirements.rkt"
         "private/utilities.rkt")

(provide
 (contract-out
  [ conviction (->* (series? exact-positive-integer?)
                    (#:dates dateset?)
                    series?)]))


(define (conviction s N #:dates (dts (current-dates)))

  (define-values (dv pre-vv fd out-v) (common-setup s dts))
  (define sigv (signalify-vector-copy pre-vv))

  ; remove too-close neighbors
  (for/fold ((last-ndx -100000))
            ((ndx (in-naturals))
             (sig (in-vector sigv))
             #:when sig)
    (when (<= (- ndx last-ndx) N)
      (vector-set! sigv last-ndx #f)
      (vector-set! sigv ndx #f))
    ndx)

  ; warps signals by N
  (when (< N (vector-length dv))
    (for ((sig (in-vector sigv))
          (dt (in-vector dv N))
          #:when sig)
      (vector-set! out-v (- dt fd) sig)))

  (make-vector-series
   #:name (format "(conviction ~a ~a)" (abbreviate s) N)
   #:vector out-v
   #:first-date fd))





;============================================

(module+ main
  (require "private/test-support.rkt"
           "signals.rkt"
           "series-count.rkt"
           "momentum.rkt")

  (with-dates TEST-SERIES
    (define mo5 (mo TEST-SERIES 5))
    (define signals (to-signals mo5))

    (dump mo5 signals (conviction signals 1) (conviction signals 2))

    (printf "\n1000 iterations: ")
    (time
     (for ((i 1000))
       (conviction signals 1)))))


;===============================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "private/test-support.rkt"
           "signals.rkt"
           "momentum.rkt")
  
  (define signals
    (with-dates TEST-SERIES
      (to-signals (mo TEST-SERIES 5))))

  (with-dates TEST-SERIES

    (check-not-exn
     (λ ()
       (verify-equivalency
        (conviction (mo TEST-SERIES 5) 4)
        MO-5-CONVICTION-4-SERIES)))

    (check-equal? (series-count signals) 147)
    
    (check-equal? (for/list ((i (in-range 1 5)))
                    (series-count (conviction signals i)))
                  '(80 53 43 38))

    ; Finally conviction happens only N periods following a signal.
    ; Thus, anywhere a signal exists in a conviction signal, N periods
    ; prior there must be that signal in the original signal series.

    (check-not-exn
     (λ ()
       (let ((cv3-f (series-function (conviction signals 3)))
             (cv2-f (series-function (conviction signals 2)))
             (cv1-f (series-function (conviction signals 1)))
             (sig-f (series-function signals))
             (dv (dateset-vector (current-dates))))
         (for ((dt+3 (in-vector dv 3))
               (dt+2 (in-vector dv 2))
               (dt+1 (in-vector dv 1))
               (dt+0 (in-vector dv 0))
               #:when (cv3-f dt+3))
           (unless
               (and (eqv? (cv3-f dt+3) (sig-f dt+0))
                    (eqv? (cv2-f dt+2) (sig-f dt+0))
                    (eqv? (cv1-f dt+1) (sig-f dt+0)))
             (raise-user-error "conviction failure"))))))))
