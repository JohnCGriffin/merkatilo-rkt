#lang racket/base

;; Convenience functions and macros for application use, not
;; consumption within library.  In particular, the in-dates macro
;; is handy but lower performance than using the more verbose
;; (in-vector (dateset-vector ...)) call.

(require (only-in racket/contract contract-out -> or/c)
	 (only-in "private/contracts.rkt" series-name/c S)
         "core/series.rkt"
         "load.rkt"
         "prepend.rkt"
         "core/dates.rkt")

(provide
 in-dates
 lo-set-dates
 (contract-out
  [ lo-or-not (-> series-name/c (or/c #f S)) ]
  [ lo-prepended (-> series-name/c #:with-surrogate-id series-name/c S) ]))

(define-syntax lo-set-dates
  (syntax-rules ()
   [ (lo-set-dates NAME)
     (let ((tmp (lo NAME)))
       (current-dates (->dateset tmp))
       tmp)]
   [ (lo-set-dates NAME arg ...)
     (let* ((tmp (lo NAME))
	    (dts (dates arg ...)))
       (current-dates dts)
       tmp) ]))


(define-syntax in-dates
  (syntax-rules ()
    [ (in-dates DTS)
      (in-vector (dateset-vector DTS)) ]
    [ (in-dates)
      (in-vector (dateset-vector (current-dates))) ]))

(define (lo-prepended id #:with-surrogate-id with-surrogate-id)
  (define primary (lo id))
  (define surrogate (lo with-surrogate-id))
  (define common-dates (dates primary surrogate #:union #t))
  (prepend primary #:with-surrogate surrogate #:dates common-dates))

(define (lo-or-not id)
  (with-handlers ([ exn:fail? not ])
    (lo id)))


