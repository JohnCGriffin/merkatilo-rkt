#lang racket/base

(require "private/common-requirements.rkt")

(provide
 (contract-out
  [ constant (-> real? S)]))


(define (constant N)
  (let ((N (* 1.0 N)))
  (series
   (λ (_) N)
   (format "constant-series-~a" N))))

