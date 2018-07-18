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
          (define result (thunk))
          (when (series? result)
            (let ((sf (series-function result)))
              (for ((dt (in-vector (dateset-vector (current-dates)))))
                (sf dt)))))
        (current-inexact-milliseconds)))
    (define ms/op (/ (- end start) 100))
    (inexact->exact (round (* ms/op 1000))))

  (define reusable-signal
    (with-dates BENCHMARK-SERIES (ema BENCHMARK-SERIES 10)))
  (define twos
    (constant 2.0))

  (define benchmarks
    (sort
     (list
      (bench "fudge"
             (let ((dts (dates (date-range '1980-1-1 '2020-1-1))))
               (λ () (with-dates dts (fudge BENCHMARK-SERIES)))))
      (bench "min-max" (λ () (min-ob BENCHMARK-SERIES)))
      (bench "math-div" (λ () (div BENCHMARK-SERIES twos)))
      (bench "math-mul" (λ () (mul BENCHMARK-SERIES twos)))
      (bench "math-add" (λ () (add BENCHMARK-SERIES twos)))
      (bench "signals" (λ () (to-signals reusable-signal)))
      (bench "volatility" (λ () (volatility BENCHMARK-SERIES)))
      (bench "reversal" (λ () (reversals BENCHMARK-SERIES #:up-factor 1.11 #:down-factor 0.9)))
      (bench "equity" (λ () (equity-line BENCHMARK-SERIES reusable-signal)))
      (bench "repeated" (λ () (repeated reusable-signal)))
      (bench "unrepeated" (λ () (unrepeated BENCHMARK-SERIES)))
      (bench "conviction" (λ () (conviction reusable-signal 2)))
      (bench "dd" (λ () (series-drawdown BENCHMARK-SERIES)))
      (bench "ema" (λ () (ema BENCHMARK-SERIES 10)))
      (bench "cross"
             (λ () (cross #:slower reusable-signal
                          #:faster BENCHMARK-SERIES)))
      (bench "warp" (λ () (warp BENCHMARK-SERIES 10)))
      (bench "ma" (λ () (ma BENCHMARK-SERIES 10)))
      (bench "mo-days" (λ () (mo-days BENCHMARK-SERIES 10)))
      (bench "mo" (λ () (mo BENCHMARK-SERIES 10))))
     string<?
     #:key bench-name))

  (with-dates BENCHMARK-SERIES
    (for ((b (in-list benchmarks)))
      (let ((name (substring (format "~a              " (bench-name b)) 0 12))
            (duration (nanos (bench-thunk b))))
        (printf "~a\t~a\n" name duration)))))

(module+ main
  (require "private/test-support.rkt")
  (standard-bench))

