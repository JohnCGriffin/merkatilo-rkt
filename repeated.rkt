#lang racket/base

(require "private/common-requirements.rkt"
         "first-last-ob.rkt"
         (only-in racket/unsafe/ops unsafe-vector-set!))

(provide
 (contract-out
  [ repeated (->* (S) (#:repeat-last boolean? #:dates DS) S) ]))


(define (repeated s #:dates (dts (current-dates)) #:repeat-last (repeat-last #f))
  
  (define dv (dateset-vector dts))
  (define vv (series-dates-values s dv))
  (define-values (fd out-v)
    (dates-appropriate-fd-and-vec dts))

  (define boundary-date
    (if repeat-last
        (add1 (last-date dts))
        (add1 (ob-d (last-ob s #:dates dts)))))


  (for/fold ((last #f))
            ((dt (in-vector dv))
             (val (in-vector vv))
             #:when (< dt boundary-date))
    (define out-val (or val last))
    (and (unsafe-vector-set! out-v (- dt fd) out-val) out-val))

  (make-vector-series
   #:name (format "(repeated ~a)" (abbreviate s))
   #:vector out-v
   #:first-date fd))



;============================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "private/test-support.rkt")
  
  (with-dates TEST-SERIES    
    (check-equal? (series-count TEST-SERIES) 754)
    (check-equal?
     (series-count TEST-SERIES)
     (series-count (repeated TEST-SERIES))))

  (with-dates (dates #:first '2013-1-3 #:last '2020-1-1)
    (check-equal?
     (series-count (repeated TEST-SERIES #:repeat-last #t))
     (vector-length (dateset-vector (current-dates))))))



