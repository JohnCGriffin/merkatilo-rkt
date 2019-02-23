#lang racket/base

(provide series-dates-values)

(require
 (combine-in
  (only-in racket/vector vector-map)
  "../core/series.rkt"
  "vector-series.rkt"))

(define (snag-value-vector dv s)
  (let ((fd (dated-series-first-date s))
        (ld (dated-series-last-date s))
        (vec (vector-series-vec s))
        (out-v (make-vector (vector-length dv))))
    (for ((dt (in-vector dv))
          (ndx (in-naturals)))
      (define val
        (and (<= fd dt)
             (<= dt ld)
             (vector-ref vec (- dt fd))))
      (vector-set! out-v ndx val))
    (vector->immutable-vector out-v)))

(define (series-dates-values s dv)
  (define cache (and (vector-series? s)
                     (vector-series-cache s)))
  (vector->immutable-vector
   (cond
     ((and cache
           (eq? (value-cache-dv cache) dv))
      (value-cache-vv cache))
     ((vector-series? s)
      (define snagged (snag-value-vector dv s))
      (define cache (value-cache dv snagged))
      (set-vector-series-cache! s cache)
      snagged)
     (else
      (vector-map (series-function s) dv)))))


