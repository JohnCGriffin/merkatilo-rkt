#lang racket/base

(require "private/common-requirements.rkt")

(provide
 (contract-out
  [ series-count (->* (S) (#:dates DS) exact-nonnegative-integer?)]))


(define (series-count s #:dates (dts (current-dates)))
  (define vv (series-dates-values s (dateset-vector dts)))
  (for/sum ((num (in-vector vv))
	    #:when num) 1))

