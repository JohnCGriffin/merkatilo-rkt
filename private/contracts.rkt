#lang racket/base

(provide (all-defined-out))

(require (only-in racket/contract or/c ->* -> cons/c and/c >/c flat-named-contract)
         (only-in racket/list empty?)
         "../core/series.rkt"
         "../core/jdate.rkt"
         "../core/dates.rkt")

(define optional-observation/c (or/c #f observation?))

(define observation-list/c (or/c (cons/c optional-observation/c list?)
                                 empty?))

(define binop/c (-> (or/c series? real?)
                    (or/c series? real?)
                    series?))

(define period/c
  (flat-named-contract
   'period
   (lambda (N) (and (integer? N)
		    (< -10000 N 10000)))))

(define positive-period/c
  (flat-named-contract
   'positive-period
   (lambda (N) (and (integer? N)
		    (< N 10000)))))

(define periodic/c (->* (series? positive-period/c)
                        (#:dates dateset?)
                        series?))

(define series-name/c (or/c string? symbol?))

(define S (flat-named-contract 'series series?))
(define DS (flat-named-contract 'dateset dateset?))
(define OB (flat-named-contract 'observation observation?))

(define nonzero-real?
  (flat-named-contract
   'nonzero-real
   (λ (x) (and (real? x) (positive? x)))))

(define positive-real? (and/c real? positive?))

(define datespec? (or/c symbol? jdate?))

(define optional-real? (or/c #f real?))

(define positive-integer? (and/c integer? positive?))

