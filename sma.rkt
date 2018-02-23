#lang racket/base

(require
 (combine-in
  "private/common-requirements.rkt"
  (only-in "private/contracts.rkt" periodic/c))
 (rename-in racket/unsafe/ops
            [unsafe-vector-ref vector-ref]
            [unsafe-vector-set! vector-set!]
            [unsafe-fx- fx-]))
            

(provide (contract-out [ sma periodic/c ]))

(define (sma s N #:dates (dts (current-dates)))

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
   #:name (format "(sma ~a ~a)" (abbreviate s) N)
   #:vector out-v
   #:first-date fd))






;=================================================

(module+ main 
  (require "private/test-support.rkt")
  (typical-run
   (λ () TEST-SERIES)
   (λ () (sma TEST-SERIES 3))))


;=====================================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "first-last-ob.rkt"
           "private/test-support.rkt")
  
  (with-dates TEST-SERIES

    (check-not-exn
     (λ ()
       (verify-equivalency (sma TEST-SERIES 3) SMA-3-SERIES)))
    
    (check-equal?
     (+ 2 (series-count (sma TEST-SERIES 3)))
     (series-count TEST-SERIES))

    (check-equal?
     (ob-v (last-ob (sma TEST-SERIES 3)))
     (/ (+ 340 337 337) 3.0))))


