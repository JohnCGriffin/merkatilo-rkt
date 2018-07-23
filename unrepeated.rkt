#lang racket/base

(require "private/common-requirements.rkt"
         (only-in racket/unsafe/ops unsafe-vector-set!))

(provide
 (contract-out
  [ unrepeated (->* (S) (#:dates DS) S)]))


(define (unrepeated s #:dates (dts (current-dates)))

  (define-values (dv vv fd out-v) (common-setup s dts))

  (for/fold ((prev #f))
            ((dt (in-vector dv))
             (val (in-vector vv)))
    (unless (eqv? prev val)
      (unsafe-vector-set! out-v (- dt fd) val))
    val)

  (make-vector-series
   #:name "unrepeated"
   #:vector out-v
   #:first-date fd))




;========================================

(module* main #f
  (require "private/test-support.rkt")
  (typical-run
   (λ () TEST-SERIES)
   (λ () (unrepeated TEST-SERIES))))



;============================================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "series-count.rkt"
           "private/test-support.rkt")

  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES
       (verify-equivalency (unrepeated TEST-SERIES)
                           (unrepeated (unrepeated TEST-SERIES))))))

  (check-true
   (with-dates TEST-SERIES
     (> (series-count TEST-SERIES)
        (series-count (unrepeated TEST-SERIES)))))

  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES
       (define sf (series-function TEST-SERIES))
       (define uf (series-function (unrepeated TEST-SERIES)))
       (define dv (dateset-vector (current-dates)))
       (for ((prev (in-vector dv))
             (curr (in-vector dv 1)))
         (cond
           ((and (eqv? (sf curr)
                       (sf prev))
                 (uf curr))
            (raise-user-error "unexpected repeat found at ~a"
                              (jdate->text curr)))
           ((and (not (eqv? (sf curr)
                            (sf prev)))
                 (not (uf curr)))
            (raise-user-error "missing value in unrepeated at ~a"
                              (jdate->text curr)))))))))
