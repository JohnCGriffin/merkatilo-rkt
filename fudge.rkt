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
        (let loop ((offset 1))
          (and (<= offset days)
               (or (sf (- dt offset))
                   (loop (add1 offset)))))))
  (series fudge-function
          (format "(fudge ~a)" (abbreviate s))))



;=================================
(module* main #f
  (require "private/test-support.rkt"
           "constant.rkt")
  (typical-run (λ () (constant 1))
               (λ () TEST-SERIES)
               (λ () (fudge TEST-SERIES))))

