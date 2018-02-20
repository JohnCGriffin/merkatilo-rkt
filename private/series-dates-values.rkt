#lang racket/base

(provide series-dates-values)

(require
 (combine-in
  (only-in racket/vector vector-map)
  racket/unsafe/ops
  "../core/series.rkt"
  "vector-series.rkt"))

(define (series-dates-values s dv)
  (vector->immutable-vector
   (if (vector-series? s)
       (let ((fd (dated-series-first-date s))
             (ld (dated-series-last-date s))
             (vec (vector-series-vec s))
             (out-v (make-vector (vector-length dv))))
         (for ((dt (in-vector dv))
               (ndx (in-naturals)))
           (define val
             (and (<= fd dt ld)
                  (unsafe-vector-ref vec (- dt fd))))
           (unsafe-vector-set! out-v ndx val))
         out-v)
       (vector-map (series-function s) dv))))

