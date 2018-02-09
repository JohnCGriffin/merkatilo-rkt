#lang racket/base

;; The main data structure in merkatilo 'series' which
;; answers a date query with either a real or false.  Series
;; are sometimes built from or decomposed into lists of observation.

(provide (struct-out observation)
         (struct-out series)
         (struct-out dated-series)
         (rename-out
          [ observation-date ob-d ]
          [ observation-value ob-v ]))


(require "jdate.rkt")

(struct observation (date value)
	#:guard (λ (date value name)
		  (if (and (jdate? date)
			   (real? value))
		      (values date value)
		      (raise-user-error 'observation "expected jdate real")))
	#:transparent)

(struct series (function name)
  #:guard (λ (function name typename)
            (define problem
              (cond
                ((or (not (procedure? function))
                     (not (procedure-arity-includes? function 1)))
                 "first argument must be (-> jdate? number?) function")
                ((not (string? name)) "second argument must be string name")
                (else #f)))
            (if problem
                (raise-user-error typename problem)
                (values function name)))
  #:transparent)

(struct dated-series series (first-date last-date)
  #:guard (λ (f n first-date last-date typename)
            (if (and (jdate? first-date)
                     (jdate? last-date)
                     (>= last-date first-date))
                (values f n first-date last-date)
                (raise-user-error typename "3rd and 4th arguments should be ordered dates")))
  #:transparent)





;==========================================

(module+ test

  (require rackunit)

  (define SAMPLE-DATE (->jdate '2000-1-1))

  (check-exn exn:fail?
             (λ () (dated-series not "misordered dates" (add1 SAMPLE-DATE) SAMPLE-DATE))))



