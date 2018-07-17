#lang racket/base

(require "main.rkt"
         "private/common-requirements.rkt"
         "private/test-support.rkt")

(struct bench (name thunk))

(define (standard-bench)
  
  (define (nanos thunk)
    (define start (current-inexact-milliseconds))
    (define end
      (begin
        (for ((i (in-range 100)))
          (thunk))
        (current-inexact-milliseconds)))
    (define ms/op (/ (- end start) 100))
    (inexact->exact (round (* ms/op 1000))))

  (define benchmarks
    (sort
     (list
      (bench "ema" (λ () (ema BENCHMARK-SERIES 10)))
      (bench "cross"
             (let ((slow (with-dates BENCHMARK-SERIES
                           (ema BENCHMARK-SERIES 10))))
               (λ () (cross #:slower slow #:faster BENCHMARK-SERIES))))
      (bench "warp" (λ () (warp BENCHMARK-SERIES 10)))
      (bench "ma" (λ () (ma BENCHMARK-SERIES 10)))
      (bench "mo" (λ () (mo BENCHMARK-SERIES 10))))
     string<?
     #:key bench-name))

  (with-dates BENCHMARK-SERIES
    (for ((b (in-list benchmarks)))
      (printf "~a\t~a\n" (bench-name b) (nanos (bench-thunk b))))))

(module+ main
  (require "private/test-support.rkt")
  (standard-bench))

