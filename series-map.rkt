#lang racket/base


(require
 (combine-in
  (only-in racket/match match)
  "private/common-requirements.rkt"))

(provide
 (contract-out
  [ series-map (->* (procedure? series?)
                    (#:missing-data-permitted boolean?)
                    #:rest (listof series?)
                    series?)]))


(define (series-map-1 proc s
                      #:missing-data-permitted missing-data-permitted)

  (define sf (series-function s))
  (define (calculator dt)
    (define val (sf dt))
    (and (or missing-data-permitted
             val)
         (proc val)))
  (series calculator  "to be tossed"))

(define (series-map-2 proc a b 
                      #:missing-data-permitted missing-data-permitted)
  (define af (series-function a))
  (define bf (series-function b))
  (define (calculator dt)
    (define a-val (af dt))
    (define b-val (bf dt))
    (and (or missing-data-permitted
             (and a-val b-val))
         (proc a-val b-val)))
  (series calculator "to be tossed"))

(define (series-map-+ proc series-list
                      #:missing-data-permitted missing-data-permitted)
  (define cache (make-hasheqv))
  (define funcs (map series-function series-list))
  (define (calculator dt)
    (hash-ref! cache
               dt
               (λ ()
                 (define nums (for/list ((f (in-list funcs)))
                                (f dt)))
                 (and (or missing-data-permitted
                          (andmap number? nums))
                      (apply proc nums)))))
  (series calculator "to be tossed"))


(define (series-map proc
                    #:missing-data-permitted (missing-data-permitted #f)
                    . series-list)

  (unless (procedure-arity-includes? proc (length series-list))
    (raise-user-error 'series-map
                      "~a cannot accept ~a series arguments"
                      proc (length series-list)))
  (rename-series
   (match series-list
     [(list a)
      (series-map-1 proc a
                    #:missing-data-permitted missing-data-permitted)]
     [(list a b)
      (series-map-2 proc a b 
                    #:missing-data-permitted missing-data-permitted)]
     [_
      (series-map-+ proc series-list
                    #:missing-data-permitted missing-data-permitted)])
   (format "(series-map/~a-series)" (length series-list))))




;============================================================

(module+ test
  (require rackunit
           "constant.rkt"
           "private/test-support.rkt")

  (define (count-missing-values permit-missing)
    (with-dates (dates TEST-SERIES #:expanded #t)
      (define missing-values-series
        (series-map (λ (val) (if val 0 1))
                    TEST-SERIES
                    #:missing-data-permitted permit-missing))
      (define mf (series-function missing-values-series))
      (for/sum ((dt (in-vector (dateset-vector (current-dates)))))
        (define val (mf dt))
        (or val 0))))

  (check-equal?
   (count-missing-values #t)
   340)

  (check-equal?
   (count-missing-values #f)
   0)

  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES ; X - X = 0
       (verify-equivalency (series-map - TEST-SERIES TEST-SERIES)
                           (constant 0)))))

  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES ; X - X - X = -X
       (verify-equivalency (series-map - TEST-SERIES TEST-SERIES TEST-SERIES)
                           (series-map - TEST-SERIES)))))

  (check-not-exn
   (λ ()
     (with-dates TEST-SERIES ; sqrt(X*X) = X
       (verify-equivalency (series-map sqrt (series-map * TEST-SERIES TEST-SERIES))
                           TEST-SERIES)))))
