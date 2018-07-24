#lang racket/base

(require "main.rkt"
         "private/test-support.rkt")

(struct bench (name thunk))

(define (standard-bench)

  (define ITERATIONS 1000)
  
  (define (ops/second thunk)
    (define start (current-inexact-milliseconds))
    (define end
      (begin
        (for ((i (in-range ITERATIONS)))
          (define result (thunk))
          (when (series? result)
            (let ((sf (series-function result)))
              (for ((dt (in-vector (dateset-vector (current-dates)))))
                (sf dt)))))
        (current-inexact-milliseconds)))
    (define seconds (/ (- end start) 1000))
    (inexact->exact (round (/ ITERATIONS seconds))))

  (define reusable-ema
    (with-dates BENCHMARK-SERIES
      (ema BENCHMARK-SERIES 10)))
  (define reusable-signal
    (with-dates BENCHMARK-SERIES
      (to-signals (cross #:slower reusable-ema
                         #:faster BENCHMARK-SERIES))))
  (define twos
    (constant 2.0))

  (define benchmarks
    (sort
     (list
      (bench "prepend"
             (λ () (prepend TEST-SERIES
                            #:with-surrogate BENCHMARK-SERIES)))
      (bench "fudge" (λ () (fudge BENCHMARK-SERIES)))
      (bench "min-max" (λ () (min-ob BENCHMARK-SERIES)))
      (bench "math-add" (λ () (add BENCHMARK-SERIES twos)))
      (bench "signals" (λ () (to-signals reusable-signal)))
      (bench "volatility" (λ () (volatility BENCHMARK-SERIES)))
      (bench "reversal"
             (λ () (reversals BENCHMARK-SERIES
                              #:up-factor 1.11
                              #:down-factor 0.9)))
      (bench "equity"
             (λ () (equity-line BENCHMARK-SERIES reusable-signal)))
      (bench "repeated" (λ () (repeated reusable-signal)))
      (bench "unrepeated" (λ () (unrepeated BENCHMARK-SERIES)))
      (bench "conviction" (λ () (conviction reusable-signal 2)))
      (bench "dd" (λ () (series-drawdown BENCHMARK-SERIES)))
      (bench "ema" (λ () (ema BENCHMARK-SERIES 10)))
      (bench "cross"
             (λ () (cross #:slower reusable-ema
                          #:faster BENCHMARK-SERIES)))
      (bench "warp" (λ () (warp BENCHMARK-SERIES 10)))
      (bench "ma" (λ () (ma BENCHMARK-SERIES 10)))
      (bench "mo-days" (λ () (mo-days BENCHMARK-SERIES 10)))
      (bench "mo" (λ () (mo BENCHMARK-SERIES 10))))
     string<?
     #:key bench-name))

  (with-dates BENCHMARK-SERIES
    (for ((b (in-list benchmarks)))
      (let ((name (substring (format "~a              "
                                     (bench-name b)) 0 12))
            (ops (ops/second (bench-thunk b))))
        (printf "~a\t~a\n" name ops)))))

(module+ main
  (require "private/test-support.rkt")
  (standard-bench))

