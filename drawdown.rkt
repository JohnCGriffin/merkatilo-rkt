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

  (define reversed-mins
    (for/fold ((result '()))
              ((ob (in-list (reverse obs)))
               #:when (or (null? result)
                          (< (ob-v ob)
                             (ob-v (car result)))))
      (cons ob result)))

  (define maxs
    (reverse (for/fold ((result '()))
                       ((ob (in-list obs))
                        #:when (or (null? result)
                                   (> (ob-v ob)
                                      (ob-v (car result)))))
               (cons ob result))))


  (let loop ((maxs maxs)
             (mins reversed-mins)
             (accum (drawdown (car maxs)
                              (car maxs)))) 

    (let* ((this-max (and (pair? maxs)
                          (car maxs)))
           (this-min (and this-max
                          (pair? mins)
                          (car mins))))
      
      (cond
        
        ((not this-min)
         accum)

        ; drop earlier mins
        ((<= (ob-d this-min)
             (ob-d this-max))
         (loop maxs
               (cdr mins)
               accum))

        ; found new biggest drawdown
        ((< (/ (ob-v this-min)
               (ob-v this-max))
            (/ (ob-v (drawdown-min accum))
               (ob-v (drawdown-max accum))))
         (loop (cdr maxs)
               mins
               (drawdown this-max this-min)))

        (else
         (loop (cdr maxs)
               mins
               accum))))))



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

