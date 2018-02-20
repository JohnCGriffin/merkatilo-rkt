#lang racket/base

(require
 (combine-in
  (only-in racket/contract contract-out -> ->*)
  (only-in "private/contracts.rkt" observation-list/c S DS)
  "private/series-dates-values.rkt"
  "core/series.rkt"
  "private/vector-series.rkt"
  "core/dates.rkt"))

(provide
 (contract-out
  [ obs->series (->* (observation-list/c) (#:name string?) S)]
  [ series->obs (-> DS S observation-list/c) ]))


(define (series->obs dts s)
  (define dv (dateset-vector dts))
  (define vv (series-dates-values s dv))
  (define result
    (for/list ((dt dv)
               (val vv)
               #:when val)
      (observation dt val)))
  (if (pair? result)
      result
      (raise-user-error 'series->obs "no observations")))

; unordered obserations
(define (obs->series obs #:name (maybe-name #f))

  (define name (or maybe-name (format "unnamed-~a" (random 1000000))))
  
  ; Find first, last while verifying date ordering.
  (define-values (first-date last-date filtered-obs)
    (for/fold ((fd #f)
               (ld #f)
               (acc '()))
              ((ob (in-list obs))
               #:when ob)
      (let ((dt (ob-d ob)))
        (values (if fd (min dt fd) dt)
                (if ld (max dt ld) dt)
                (cons ob acc)))))

  (if first-date
      
      (let* ((days (add1 (- last-date first-date)))
             (vec (make-vector days #f)))
	
        (for ((ob (in-list filtered-obs)))
          (let ((val (ob-v ob))
                (ndx (- (ob-d ob) first-date)))
            (vector-set! vec ndx val)))

        (make-vector-series
         #:vector vec
         #:name name
         #:first-date first-date))
      
      (series (λ (dt) #f) name)))




;======================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "core/series.rkt"
           "core/dates.rkt")
  (define never-series
    (series (λ (dt) #f) "never.."))

  (with-dates (dates (date-range '2000-1-1 '2001-1-1))

    (check-exn
     exn?
     (λ ()
       (series->obs (current-dates) never-series)))

    (check-not-exn
     (λ ()
       (obs->series '())))))

