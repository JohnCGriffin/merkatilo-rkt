#lang racket/base


(require "private/common-requirements.rkt")

(provide
 (contract-out
  [ series-filter (-> procedure? S S) ]))

(define (series-filter proc s)
  (define sf (series-function s))
  (series
   (lambda (dt)
     (define val (sf dt))
     (and val (proc val) val))
   (format "(filter ... ~a)" (abbreviate s))))


;============================================================

(module+ test
  (require rackunit
           (only-in racket/function curryr)
	   "series-count.rkt"
           "private/test-support.rkt")

  (check-equal?
   (series-count (series-filter (curryr > 360) TEST-SERIES) #:dates (dates TEST-SERIES))
   2))
