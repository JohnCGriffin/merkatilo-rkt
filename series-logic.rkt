#lang racket/base

(require "private/common-requirements.rkt")

(provide
   (contract-out
      [ series-or  (-> S S S)]
      [ series-and (-> S S S)]))

(define (series-or a b)
  (define af (series-function a))
  (define bf (series-function b))
  (series
   (lambda (dt)
     (or (af dt) (bf dt)))
   (format "(series-or ~a ~a)" (abbreviate a) (abbreviate b))))

(define (series-and a b)
  (define af (series-function a))
  (define bf (series-function b))
  (series
   (lambda (dt)
     (and (af dt) (bf dt)))
   (format "(series-and ~a ~a)" (abbreviate a) (abbreviate b))))


;====================================
(module+ test
  (require rackunit
           "private/test-support.rkt")
  (define A (literal-series '((2012-2-1 1)
                              (2012-3-3 1)
                              (2012-3-4 1)
                              (2013-4-4 1))))
  (define B (literal-series '((2012-2-1 2)
                              (2012-2-2 2)
                              (2012-3-3 2)
                              (2013-4-1 2)
                              (2013-4-4 2))))
  (with-dates (dates #:first '2010-1-1 #:last '2020-1-1)
    (check-not-exn
    (λ ()
     (verify-equivalency (series-or A B)
                         (literal-series '((2012-2-1 1)
                                           (2012-2-2 2)
                                           (2012-3-3 1)
                                           (2012-3-4 1)
                                           (2013-4-1 2)
                                           (2013-4-4 1))))))
    (check-not-exn
    (λ ()
     (verify-equivalency (series-or B A)
                         (literal-series '((2012-2-1 2)
                                           (2012-2-2 2)
                                           (2012-3-3 2)
                                           (2012-3-4 1)
                                           (2013-4-1 2)
                                           (2013-4-4 2))))))
    (check-not-exn
    (λ ()
     (verify-equivalency (series-and A B)
                         (literal-series '((2012-2-1 2)
                                           (2012-3-3 2)
                                           (2013-4-4 2))))))))

                              
