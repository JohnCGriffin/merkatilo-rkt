#lang racket/base

(require "private/common-requirements.rkt")


(provide
 (struct-out drawdown)
 (contract-out
  [ drawdown-residual (->* (S) (#:dates DS) (>=/c 0)) ]
  [ series-drawdown   (->* (S) (#:dates DS) drawdown?) ]))



(struct drawdown (max min) #:transparent)

(define (series-drawdown s #:dates (dts (current-dates)))
  
  (define obs (series->obs dts s))
  (define r-obs (reverse obs))

  (define reversed-minima
    (for/fold ((acc (list (car r-obs))))
              ((ob (in-list r-obs))
               #:when (< (ob-v ob)
                         (ob-v (car acc))))
      (cons ob acc)))

  (define maxima
    (reverse
     (for/fold ((acc (list (car obs))))
               ((ob (in-list obs))
                #:when (> (ob-v ob)
                          (ob-v (car acc))))
       (cons ob acc))))

  (define (ob-ratio a b)
    (/ (ob-v a) (ob-v b)))

  (define (and-car p)
    (and (pair? p) (car p)))

  (let loop ((maxima maxima)
             (minima reversed-minima)
             (accum (drawdown (car maxima)
                              (car maxima)))) 

    (let* ((mx (and-car maxima))
           (mn (and-car minima)))
      
      (cond
        
        ((not (and mn mx))
         accum)

        ; drop earlier minima
        ((<= (ob-d mn)
             (ob-d mx))
         (loop maxima (cdr minima) accum))

        ; found new biggest drawdown
        ((< (ob-ratio mn mx)
            (ob-ratio (drawdown-min accum)
                      (drawdown-max accum)))
         (loop (cdr maxima) minima (drawdown mx mn)))

        (else
         (loop (cdr maxima) minima accum))))))



(define (drawdown-residual s #:dates (dts (current-dates)))
  (define dd (series-drawdown s #:dates dts))
  (/ (ob-v (drawdown-min dd))
     (ob-v (drawdown-max dd))))



;==============================================
(module+ main
  (require "private/test-support.rkt")
  (with-dates TEST-SERIES
    (define result (series-drawdown TEST-SERIES))
    (printf "(~a ~a) -> (~a ~a)\n"
            (jdate->text (ob-d (drawdown-max result)))
            (ob-v (drawdown-max result))
            (jdate->text (ob-d (drawdown-min result)))
            (ob-v (drawdown-min result)))))


;=============================================
(module+ test
  (require rackunit
           "private/test-support.rkt")

  (check-equal?
     (drawdown-residual TEST-SERIES #:dates (dates TEST-SERIES))
     (with-dates TEST-SERIES (drawdown-residual TEST-SERIES)))
  
  (with-dates TEST-SERIES
    (define result (series-drawdown TEST-SERIES))
    (define sf (series-function TEST-SERIES))
    (define dv (dateset-vector (current-dates)))
    (define biggest
      (for*/fold ((biggest 0))
                 ((d1 (in-vector dv))
                  (d2 (in-vector dv))
                  #:when (> d2 d1))
        (max (/ (sf d1)
                (sf d2)) biggest)))
    (check-equal?
     (->jdate '2014-9-18)
     (ob-d (drawdown-max result)))
    (check-equal?
     (->jdate '2014-12-16)
     (ob-d (drawdown-min result)))
    (check-equal?
     biggest
     (/ (ob-v (drawdown-max result))
        (ob-v (drawdown-min result))))))

