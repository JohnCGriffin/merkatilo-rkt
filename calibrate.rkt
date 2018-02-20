#lang racket/base

;; To calibrate a series is to make it value-compatible with another.
;; For example, a spider graph picks a point at which multiple time
;; series are the same value.
;;
;; The default date is the beginning of the dateset and the default
;; value of the series there is 100, making any value in the series
;; equivalent to the percentage of the beginning.

(require
 (combine-in
  "private/common-requirements.rkt"
  "private/contracts.rkt"))

(provide
 (contract-out
  [ calibrate
    (->* (S) (#:init nonzero-real? #:date datespec?) S)]))


(define (calibrate s
		   #:init (init 100)
		   #:date (date (first-date)))
  
  (define sf (series-function s))
  
  (define ratio
    (let* ((dt (->jdate date))
	   (val (sf dt))
	   (problem (cond
                      ((not val) "no observation at ~a")
                      ((zero? val) "value is ZERO at ~a")
                      (else #f))))
      (if problem
	  (raise-user-error 'calibrate problem (jdate->text dt))
	  (/ init val))))
  
  (series
   (lambda (dt)
     (define val (sf dt))
     (and val (* ratio val)))
   (format "(calibrate ~a)" (abbreviate s))))





;====================================

(module+ main
  (require "private/test-support.rkt")
  (typical-run (λ () (calibrate TEST-SERIES))))




;====================================

(module+ test
  (require rackunit
           "series-binop.rkt"
           "constant.rkt"
           "private/test-support.rkt")

  (check-exn
   exn?
   (λ ()
     (with-dates TEST-SERIES
       (calibrate NEVER-SERIES))))

  (check-exn
   exn?
   (λ ()
     (with-dates TEST-SERIES 
       (calibrate (constant 0)))))


  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES
       (define sf (series-function TEST-SERIES))
       (define A (calibrate TEST-SERIES))
       (define ratio (/ 100 (sf (first-date))))
       (verify-equivalency A (mul ratio TEST-SERIES)))))

  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES
       (define sf (series-function TEST-SERIES))
       (define A (calibrate TEST-SERIES #:init 22))
       (define ratio (/ 22 (sf (first-date))))
       (verify-equivalency A (mul ratio TEST-SERIES)))))
  
  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES
       (define sf (series-function TEST-SERIES))
       (define DATE (->jdate '2014-12-31))
       (define A (calibrate TEST-SERIES #:init 22 #:date '2014-12-31))
       (define ratio (/ 22 (sf DATE)))
       (verify-equivalency A (mul ratio TEST-SERIES))))))
