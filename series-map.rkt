#lang racket/base


(require
 (combine-in
  (only-in racket/match match)
  "constant.rkt"
  "private/common-requirements.rkt"))

(provide
 (contract-out
  [ series-map (->* (procedure? series?)
                    (#:missing-data-permitted boolean?)
                    #:rest (listof series?)
                    series?)]))


(define (series-map-f-1 proc s
                      #:missing-data-permitted missing-data-permitted)

  (define sf (series-function s))
  (if missing-data-permitted
      (λ (dt)
        (proc (sf dt)))
      (λ (dt)
        (define val (sf dt))
        (and val (proc val)))))

(define (series-map-f-2 proc a b 
                      #:missing-data-permitted missing-data-permitted)
  (define af (series-function a))
  (define bf (series-function b))

  ;; A common condition is math with a second constant series and
  ;; no missing-data-permitted, so optimize remove the constant series call.
  (cond
    ((and (constant-series? b)
          (not missing-data-permitted))
     (let ((C (bf MIN-DATE)))
      (λ (dt)
        (define a-val (af dt))
        (define b-val (and a-val C))
        (and b-val (proc a-val b-val)))))
    (missing-data-permitted
      (λ (dt)
        (proc (af dt) (bf dt))))
    (else
      (λ (dt)
        (define a-val (af dt))
        (define b-val (and a-val (bf dt)))
        (and b-val (proc a-val b-val))))))


(define (series-map-f-+ proc series-list
                      #:missing-data-permitted missing-data-permitted)
  (define cache (make-hasheqv))
  (define funcs (map series-function series-list))
  (λ (dt)
    (hash-ref! cache
               dt
               (λ ()
                 (define nums (for/list ((f (in-list funcs)))
                                (f dt)))
                 (and (or missing-data-permitted
                          (andmap number? nums))
                      (apply proc nums))))))


(define (series-map proc
                    #:missing-data-permitted (missing-data-permitted #f)
                    . series-list)

  (unless (procedure-arity-includes? proc (length series-list))
    (raise-user-error 'series-map
                      "~a cannot accept ~a series arguments"
                      proc (length series-list)))
  (series
   (match series-list
     [(list a)
      (series-map-f-1 proc a
                    #:missing-data-permitted missing-data-permitted)]
     [(list a b)
      (series-map-f-2 proc a b 
                    #:missing-data-permitted missing-data-permitted)]
     [_
      (series-map-f-+ proc series-list
                    #:missing-data-permitted missing-data-permitted)])
   (format "(series-map/~a-series)" (length series-list))))




;============================================================

(module* test racket/base
  (require rackunit
           (submod "..")
           (only-in racket/function identity)
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

  (check-exn
   exn?
   (λ ()
     (with-dates TEST-SERIES
       (series-map identity TEST-SERIES TEST-SERIES))))

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
