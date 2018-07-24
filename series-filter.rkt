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

(module* test racket/base
  (require rackunit
           (submod "..")
           (only-in racket/function curryr)
           "private/test-support.rkt")

  (check-equal?
   (series-count (series-filter (curryr > 360) TEST-SERIES) #:dates (dates TEST-SERIES))
   2))
