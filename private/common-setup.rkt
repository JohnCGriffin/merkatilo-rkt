#lang racket/base

(provide common-setup)

(require "vector-series.rkt"
         "series-dates-values.rkt"
         "../core/dates.rkt")

(define (common-setup s dts)
  (define dv (dateset-vector dts))
  (define vv (series-dates-values s dv))
  (define-values (fd out-v)
    (dates-appropriate-fd-and-vec dts))
  (values dv vv fd out-v))



