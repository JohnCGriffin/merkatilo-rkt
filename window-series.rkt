#lang racket/base

(require "private/common-requirements.rkt")

(provide
 (contract-out
  [ window-series (->* (S integer? (-> vector? real?))
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
             (stop (in-naturals 1))
             (val (in-vector vv)))
    
    (define ok (or val missing-data-permitted val))
    (define new-count (if ok (add1 count) 0))
    
    (when (>= new-count N)
      (vector-copy! window 0 vv (- stop N) stop)
      (vector-set! out-v
                   (- dt fd)
                   (proc window)))
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

  (define (vector-avg v)
    (/ (for/sum ((n (in-vector v))) n)
       (vector-length v)))

  (check-not-exn
   (λ ()
     (with-dates (dates #:first '2013-1-1 #:last '2013-12-31)
       (verify-equivalency
        (sma TEST-SERIES 3)
        (window-series TEST-SERIES 3 vector-avg)))))

  
  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES
       (verify-equivalency
        (sma TEST-SERIES 200)
        (window-series TEST-SERIES 200 vector-avg))))))

