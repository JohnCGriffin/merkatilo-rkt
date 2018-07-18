#lang racket/base

(require "private/common-requirements.rkt")

(provide
 (struct-out constant-series)
 (contract-out
  [ constant (-> real? S)]))

(struct constant-series series ())

(define (constant N)
  (let ((N (* 1.0 N)))
  (constant-series
   (λ (_) N)
   (format "constant-series-~a" N))))

