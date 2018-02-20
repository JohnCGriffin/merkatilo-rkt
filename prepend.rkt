#lang racket/base

(require "private/common-requirements.rkt")

(provide
 (contract-out
  [ prepend (->* (S #:with-surrogate series?) (#:dates DS) S)]))


(define (prepend primary
                 #:with-surrogate surrogate
                 #:dates (dates (current-dates)))

  (define dv (dateset-vector dates))
  (define lf (series-function primary))
  (define sf (series-function surrogate))
  
  (define common-ob
    (or (for/first ((dt (in-vector dv))
                    #:when (and (lf dt)
                                (sf dt)))
          (observation dt (/ (lf dt)
                             (sf dt))))
        (raise-user-error 'overlay "no common dates")))

  (define ratio (ob-v common-ob))

  (series
   (λ (dt)
     (or (lf dt)
         (let ((val (sf dt)))
           (and val
                (* val ratio)))))
   (format "(prepend ~a #:with-surrogate ~a)"
           (abbreviate primary)
           (abbreviate surrogate))))



;===========================================

(module+ main
  (require "dump.rkt"
           "private/test-support.rkt")
  (define EARLIER (literal-series '((2013-1-1 80) (2013-2-1 90) (2013-3-1 100))))
  (define LATER (literal-series '((2013-3-1 1000) (2013-6-1 1130))))
  (with-dates (dates #:first '2013-1-1 #:last '2015-1-1)
    (dump EARLIER LATER (prepend LATER #:with-surrogate EARLIER))))




;===========================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "private/test-support.rkt")
  (with-dates (dates #:first '2012-1-1 #:last '2015-1-1)
    (define EARLIER (literal-series '((2013-1-1 80) (2013-2-1 90) (2013-3-1 100))))
    (define LATER (literal-series '((2013-3-1 1000) (2013-6-1 1130))))
    (define TOO-MUCH-LATER (literal-series '((2014-1-1 10000) (2016-1-1 11111))))
    (check-exn
     exn?
     (λ ()
       (with-dates (dates #:first '2000-1-1 #:last (today))
         (prepend TOO-MUCH-LATER #:with-surrogate EARLIER))))
    (check-not-exn
     (λ ()
       (verify-equivalency
        (prepend LATER #:with-surrogate EARLIER)
        (literal-series '((2013-1-1 800) (2013-2-1 900) (2013-3-1 1000) (2013-6-1 1130))))))))


