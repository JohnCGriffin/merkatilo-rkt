#lang racket/base


(require
 (combine-in
  "private/common-requirements.rkt"
  (only-in "private/contracts.rkt" periodic/c)
  "fudge.rkt"
  "repeated.rkt"))


(provide
 (contract-out
  [ mo-days periodic/c ]
  [ mo periodic/c ] 
  [ mo% periodic/c ]))


(define (change val old-val)

  ; calculate only over positive values
  (define ratio
    (and val
         old-val
         (> val 0)
         (> old-val 0)
         (/ val old-val)))

  (and ratio (- ratio 1.0)))


(define (mo-worker s N dts)
  (define-values (dv vv fd out-v) (common-setup s dts))

  (when (< N (vector-length vv))
    (for ((v0 (in-vector vv))
          (v1 (in-vector vv N))
          (dt (in-vector dv N))
          #:when (and v1 v0 (not (zero? v0))))
      (define result (change v1 v0))
      (when result
        (vector-set! out-v (- dt fd) result))))
  (values out-v fd))

(define (mo s N #:dates (dts (current-dates)))

  (define-values (vec fd)
    (mo-worker s N dts))
  
  (make-vector-series
   #:name (format "(mo ~a ~a)" (abbreviate s) N)
   #:vector vec
   #:first-date fd))

(define (mo% s N #:dates (dts (current-dates)))
  
  (define-values (vec fd)
    (mo-worker s N dts))
  
  (for ((ndx (in-range (vector-length vec))))
    (define val (vector-ref vec ndx))
    (and val (vector-set! vec ndx (* 100.0 val))))
  
  (make-vector-series
   #:name (format "(mo% ~a ~a)" (abbreviate s) N)
   #:vector vec
   #:first-date fd))

(define (mo-days s days #:dates (dts (current-dates)))
  (define-values (dv vv fd out-v) (common-setup s dts))
  (define ff (series-function (fudge s)))

  (for ((val (in-vector vv))
        (dt (in-vector dv))
        #:when val)
    (define result (change val (ff (- dt days))))
    (when result
      (vector-set! out-v (- dt fd) result)))

  (make-vector-series
   #:name (format "(mo-days ~a ~a)" (abbreviate s) days)
   #:vector out-v
   #:first-date fd))




;===================================

(module* main #f
  (require "private/test-support.rkt"
           "constant.rkt")
  (typical-run (λ () TEST-SERIES)
               (λ () (constant 1))
               (λ () (mo TEST-SERIES 5))
               (λ () (mo% TEST-SERIES 5))
               (λ () (mo-days TEST-SERIES 5))))


;============================================================

(module+ test
  (require rackunit
           "series-binop.rkt"
           "warp.rkt"
           "private/test-support.rkt")

  (with-dates TEST-SERIES

    (check-not-exn
     (λ ()
       (verify-equivalency (mo TEST-SERIES 3) MO-3-SERIES)))

    (check-not-exn
     (λ ()
       (define MO1 (mo TEST-SERIES 1))
       (define ratio (div TEST-SERIES (warp TEST-SERIES 1)))
       (define manually (sub ratio 1.0))
       (verify-equivalency MO1 manually)))))

