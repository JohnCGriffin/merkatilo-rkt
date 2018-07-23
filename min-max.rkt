#lang racket/base

;; Find the first minimum and first maximum observations.  If no
;; observations are available in the dateset, an error is raised.

(require "private/common-requirements.rkt"
	 "private/values-macros.rkt"
         "first-last-ob.rkt")

(provide
 (contract-out
  [ min-max-obs (->* (S) (#:dates DS) (values OB OB)) ]
  [ max-ob      (->* (S) (#:dates DS) OB) ]
  [ min-ob      (->* (S) (#:dates DS) OB) ]))


(define (min-max-obs s #:dates (dts (current-dates)))
  (define dv (dateset-vector dts))
  (define sv (series-dates-values s dv))
  (define ob (first-ob s #:dates dts))
  (define-values
    (min-d min-v max-d max-v)
    (for/fold ([min-d (ob-d ob)]
               [min-v (ob-v ob)]
               [max-d (ob-d ob)]
               [max-v (ob-v ob)])
              ((dt (in-vector dv))
               (val (in-vector sv))
               #:when (and val (or (< val min-v)
                                   (> val max-v))))
      (if (< val min-v)
          (values dt val max-d max-v)
          (values min-d min-v dt val))))

  (values (observation min-d min-v)
          (observation max-d max-v)))


(define (min-ob s #:dates (dts (current-dates)))
  (first-of-values (min-max-obs s #:dates dts)))

(define (max-ob s #:dates (dts (current-dates)))
  (second-of-values (min-max-obs s #:dates dts)))



;===========================================================

(module+ main
  (require "private/test-support.rkt")
  (with-dates TEST-SERIES
    (printf "1000 iterations: ")
    (time
     (for ((i 1000))
       (min-max-obs TEST-SERIES)))))


;============================================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "series-binop.rkt"
           "warp.rkt"
           "core/series.rkt"
           "private/test-support.rkt")

  (check-equal?
   (->jdate "2014-09-18")
   (observation-date (max-ob TEST-SERIES #:dates (dates TEST-SERIES))))

  (check-equal?
   (->jdate "2012-01-13")
   (observation-date (min-ob TEST-SERIES #:dates (dates TEST-SERIES))))

  (check-exn
   exn?
   (λ ()
     (min-ob TEST-SERIES #:dates (dates (date-range '1996-1-1 '1997-1-1)))))

  (with-dates TEST-SERIES

    (define-values (mn mx) (min-max-obs TEST-SERIES))

    (check-equal? mx (max-ob TEST-SERIES))

    (check-equal? mn (min-ob TEST-SERIES))
    
    (check-equal?
     (->jdate "2014-09-18")
     (observation-date (max-ob TEST-SERIES)))

    (check-equal?
     (->jdate "2012-01-13")
     (observation-date (min-ob TEST-SERIES)))))
