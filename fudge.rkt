#lang racket/base

;; To fudge a series is to create a decorator series that looks back N
;; (defaulted to 6) days for an answer to a date query.  This is handy
;; for slightly mismatched data, such as weekly or market-day series
;; being used with end-of-month series.

(require "private/common-requirements.rkt")

(provide
 (contract-out
  [ fudge (->* (S) (#:days positive-integer?) S)]))


(define (fudge s #:days (days 6))
  (define sf (series-function s))
  (define (fudge-function dt)
    (or (sf dt)
        (for/first ((i (in-range 1 (+ days 1)))
                    #:when (sf (- dt i)))
          (sf (- dt i)))))
  (series fudge-function
          (format "(fudge ~a)" (abbreviate s))))



;=================================
(module* main #f
  (require "private/test-support.rkt"
           "constant.rkt")
  (typical-run (λ () (constant 1))
               (λ () TEST-SERIES)
               (λ () (fudge TEST-SERIES))))

