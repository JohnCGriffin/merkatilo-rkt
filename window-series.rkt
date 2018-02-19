#lang racket/base

(require "private/common-requirements.rkt")

(provide
 (contract-out
  [ window-series (->* (S integer? (-> vector? integer? integer? real?))
                       (#:dates DS
                        #:missing-data-permitted boolean?)
                       S)]))

(define (window-series s N proc
                       #:dates (dates (current-dates))
                       #:missing-data-permitted (missing-data-permitted #f))
  
  (define window (make-vector N))
  (define-values (dv vv fd out-v) (common-setup s dates))

  (for/fold ((count 0))
            ((dt (in-vector dv))
             (ndx (in-naturals))
             (val (in-vector vv)))
    (define new-count (if (or missing-data-permitted val) (add1 count) 0))
    (when (>= new-count N)
      (define start (add1 (- ndx N)))
      (define stop (add1 ndx))
      (vector-copy! window 0 vv start stop)
      (define result (proc window))
      (when result
        (vector-set! out-v (- dt fd) result)))
    new-count)
  
  (make-vector-series
   #:first-date fd
   #:name (format "(window-series ~a ~a)" (abbreviate s) N)
   #:vector out-v))


;==================================================

(module+ main 
  (require "private/test-support.rkt"
           "momentum.rkt")
  (define MO1 (mo TEST-SERIES 1 #:dates (dates TEST-SERIES)))
  (define (vector-sum v) (for/sum ((n (in-vector v))) n))
  (typical-run (λ () (window-series MO1 200 vector-sum))))


;==================================================

(module+ test
  (require rackunit
           "sma.rkt"
           "private/test-support.rkt")

  
  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES
       (verify-equivalency
        (sma TEST-SERIES 200)
        (window-series TEST-SERIES 200 (λ (v)
                                        (/ (for/sum ((n (in-vector v))) n)
                                           (vector-length v)))))))))

