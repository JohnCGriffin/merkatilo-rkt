#lang racket/base

(require
 (combine-in
  "private/common-requirements.rkt"
  "first-last-ob.rkt"))

(provide
 (contract-out
  [ nostradamus (->* (S #:down-factor real? #:up-factor real?) (#:dates DS) S) ]
  [ reversals   (->* (S #:down-factor real? #:up-factor real?) (#:dates DS) S) ]))


(define (reversals-worker s
                          #:down-factor _down-factor
                          #:up-factor _up-factor
                          #:dates dts
                          #:nostradamus nostradamus)

  (define down-factor (exact->inexact _down-factor))
  (define up-factor (exact->inexact _up-factor))
  (define-values (dv vv fd out-v) (common-setup s dts))

  (define initial-ob (first-ob s #:dates dts))

  (for/fold ((min-ob initial-ob)
             (max-ob initial-ob)
             (state #f))
            ((dt (in-vector dv))
             (val (in-vector vv))
             #:when val)

    (cond
      ((and (not (eqv? 1 state))
            (> val (* (ob-v min-ob) up-factor)))
       (let ((date (if nostradamus (ob-d min-ob) dt))
             (ob (observation dt val)))
         (vector-set! out-v (- date fd) 1)
         (values ob ob 1)))

      ((and (not (eqv? -1 state))
            (< val (* (ob-v max-ob) down-factor)))
       (let ((date (if nostradamus (ob-d max-ob) dt))
             (ob (observation dt val)))
         (vector-set! out-v (- date fd) -1)
         (values ob ob -1)))

      ((< val (ob-v min-ob))
       (values (observation dt val) max-ob state))

      ((> val (ob-v max-ob))
       (values min-ob (observation dt val) state))

      (else
       (values min-ob max-ob state))))

  (make-vector-series
   #:name (format "(~a ~a ~a ~a)"
                  (if nostradamus "nostradamus" "reversals")
                  (abbreviate s)
                  up-factor
                  down-factor)
   #:vector out-v
   #:first-date fd))


(define (reversals s 
                   #:down-factor down-factor
                   #:up-factor up-factor
                   #:dates (dts (current-dates)))
  (reversals-worker s
                    #:down-factor down-factor
                    #:up-factor up-factor
                    #:dates dts
                    #:nostradamus #f))

(define (nostradamus s 
                     #:down-factor down-factor
                     #:up-factor up-factor
                     #:dates (dts (current-dates)))
  (reversals-worker s
                    #:down-factor down-factor
                    #:up-factor up-factor
                    #:dates dts
                    #:nostradamus #t))


;=================================================

(module* main #f
  (require "private/test-support.rkt"
           "series-logic.rkt"
           "dump.rkt")
  (typical-run
   #:full-dump #t
   (λ () (nostradamus TEST-SERIES #:down-factor .95 #:up-factor 1.05))
   (λ () (reversals TEST-SERIES #:down-factor .95 #:up-factor 1.05))))


;=========================================
(module* test racket/base
  (require rackunit
           (submod "..")
           "private/test-support.rkt")

  (with-dates TEST-SERIES

    (check-not-exn
     (λ ()
       (verify-equivalency
        (reversals TEST-SERIES #:down-factor .95 #:up-factor 1.05)
        REVERSALS-95-105-SERIES)))

    
    (check-not-exn
     (λ ()
       (verify-equivalency
        (reversals TEST-SERIES #:down-factor .91 #:up-factor 1.09)
        REVERSALS-91-109-SERIES)))))

