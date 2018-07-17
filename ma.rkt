#lang racket/base

(require
 (combine-in
  "private/common-requirements.rkt"
  (only-in "private/contracts.rkt" periodic?))
 (rename-in racket/unsafe/ops
            [unsafe-vector-ref vector-ref]
            [unsafe-vector-set! vector-set!]
            [unsafe-fx- fx-]))
            

(provide (contract-out [ ma periodic? ]
                       [ sma periodic? ]))

(define (ma s N #:dates (dts (current-dates)))

  (define-values (dv vv fd out-v) (common-setup s dts))

  (define fN (exact->inexact N))
  
  (for/fold ((total 0.0)
             (consecutive 0))
            ((ndx (in-naturals))
             (dt (in-vector dv))
             (val (in-vector vv)))
    (if (not val)
        (values 0.0 0)
        (let* ((to-drop (if (>= consecutive N)
                            (vector-ref vv (- ndx N))
                            0))
               (new-total (- (+ total val) to-drop)))
          (when (>= consecutive (sub1 N))
            (vector-set! out-v (fx- dt fd) (/ new-total fN)))
          (values new-total (add1 consecutive)))))

  (make-vector-series
   #:name (format "(ma ~a ~a)" (abbreviate s) N)
   #:vector out-v
   #:first-date fd))


; temporary compatibility
(define sma ma)



;=================================================

(module+ main 
  (require "private/test-support.rkt")
  (typical-run
   (λ () TEST-SERIES)
   (λ () (ma TEST-SERIES 3)))
  (typical-benchmark (λ () (ma BENCHMARK-SERIES 10))))


;=====================================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "first-last-ob.rkt"
           "private/test-support.rkt")
  
  (with-dates TEST-SERIES

    (check-not-exn
     (λ ()
       (verify-equivalency (ma TEST-SERIES 3) MA-3-SERIES)))
    
    (check-equal?
     (+ 2 (series-count (ma TEST-SERIES 3)))
     (series-count TEST-SERIES))

    (check-equal?
     (ob-v (last-ob (ma TEST-SERIES 3)))
     (/ (+ 340 337 337) 3.0))))


