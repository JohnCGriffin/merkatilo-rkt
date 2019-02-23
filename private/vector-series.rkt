#lang racket/base

(require "../core/series.rkt"
         "../core/dates.rkt"
         "../core/jdate.rkt"
         (only-in racket/contract contract-out ->))

(provide
 (except-out (struct-out vector-series) vector-series)
 (contract-out
  [ dates-appropriate-fd-and-vec (-> dateset?
                                     (values integer? vector?)) ]
  [ make-vector-series (-> #:first-date integer?
                           #:vector vector?
                           #:name string?
                           vector-series?)]))

(provide (struct-out value-cache))

(struct value-cache (dv vv))

(struct vector-series dated-series (vec (cache #:mutable))

  #:guard
  (lambda (f n fd ld v c  type-name)
    (unless (and (vector? v)
                 (eqv? (vector-length v)
                       (add1 (- ld fd))))
      (raise-user-error "(vector-length) != (add1 (- last-date first-date))"))
    (values f n fd ld (vector->immutable-vector v) #f))

  #:methods gen:custom-write
  [(define write-proc
     (λ (instance port mode)
       (fprintf port "<~a,~a ... ~a>"
                (series-name instance)
                (jdate->text (dated-series-first-date instance))
                (jdate->text (dated-series-last-date instance))))) ])



(define (make-vector-series #:first-date fd
                            #:vector vec
                            #:name name)
  (define final-v (vector->immutable-vector vec))
  (define ld (sub1 (+ fd (vector-length vec))))
  (define (F dt)
    (and (<= fd dt)
         (<= dt ld)
         (vector-ref final-v (- dt fd))))
  (vector-series F name fd ld final-v #f))

(define (dates-appropriate-fd-and-vec dts)
  (define fd (first-date dts))
  (define ld (last-date dts))
  (define vec (make-vector (add1 (- ld fd)) #f))
  (values fd vec))
  
