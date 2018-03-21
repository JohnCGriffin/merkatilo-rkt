#lang racket/base

(require "private/common-requirements.rkt")


(provide
 (struct-out drawdown)
 (contract-out
  [ drawdown-residual (->* (S) (#:dates DS) (>=/c 0)) ]
  [ series-drawdown   (->* (S) (#:dates DS) drawdown?) ]))



(struct drawdown (max min) #:transparent)

(define (series-drawdown s #:dates (dts (current-dates)))

  (define dv (dateset-vector dts))
  (define vv (series-dates-values s dv))

  (define reversed-minima
    (let ((len (vector-length dv))
          (mn #f))
      (for/fold ((acc '()))
                ((i (in-naturals 1))
                 (val (in-vector vv (sub1 len) -1 -1))
                 #:when (and val
                             (or (not mn)
                                 (<= val mn))))
        (define dt (vector-ref dv (- len i)))
        (define ob (observation dt val))
        (set! mn val)
        (cons ob acc))))

  (define maxima
    (let ((len (vector-length dv))
          (mx #f))
      (reverse
       (for/fold ((acc '()))
                 ((i (in-naturals))
                  (val (in-vector vv))
                  #:when (and val
                              (or (not mx)
                                  (> val mx))))
         (define dt (vector-ref dv i))
         (define ob (observation dt val))
         (set! mx val)
         (cons ob acc)))))

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
            (ob-v (drawdown-min result)))
    (printf "1000 drawdowns: ")
    (time
     (for ((i 10000))
       (series-drawdown TEST-SERIES)))))

;=============================================
(module* test racket/base
  (require rackunit
           (submod "..")
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

