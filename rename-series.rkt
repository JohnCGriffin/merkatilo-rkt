#lang racket/base

(require
 (combine-in
  (only-in racket/contract contract-out -> any/c)
  "core/series.rkt"
  "private/vector-series.rkt"))

(provide
 (contract-out
  [ rename-series (-> series? string? series?)]))

(define (rename-series s name)
  (if (vector-series? s)
      (make-vector-series #:name name
                          #:vector (vector-series-vec s)
                          #:first-date (dated-series-first-date s))
      (struct-copy series s [ name name ])))
