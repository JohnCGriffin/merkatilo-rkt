#lang racket/base

(require
 (combine-in
  "private/common-requirements.rkt"
  "first-last-ob.rkt"))

(provide
 (contract-out
  [ nostradamus (->* (S #:down-factor real? #:up-factor real?) (#:dates DS) S) ]
  [ reversals   (->* (S #:down-factor real? #:up-factor real?) (#:dates DS) S) ]))

(struct mms (min-d min-v max-d max-v mode))

(define (reversals-worker s
                          #:down-factor _down-factor
                          #:up-factor _up-factor
                          #:dates dts
                          #:nostradamus nostradamus)

  (define down-factor (exact->inexact _down-factor))
  (define up-factor (exact->inexact _up-factor))
  (define-values (dv vv fd out-v) (common-setup s dts))

  (define init-ob (first-ob s #:dates dts))

  (for/fold ((state (mms (ob-d init-ob)
                         (ob-v init-ob)
                         (ob-d init-ob)
                         (ob-v init-ob)
                         #f)))
            ((dt (in-vector dv))
             (val (in-vector vv))
             #:when val)

    (let ((min-val (mms-min-v state))
          (max-val (mms-max-v state))
          (mode (mms-mode state)))
      
      (cond
        ((and (not (eqv? 1 mode))
              (> val (* min-val up-factor)))
         (let ((date (if nostradamus (mms-min-d state) dt)))
           (vector-set! out-v (- date fd) 1)
           (mms dt val dt val 1)))


        ((and (not (eqv? -1 mode))
              (< val (* max-val down-factor)))
         (let ((date (if nostradamus (mms-max-d state) dt)))
           (vector-set! out-v (- date fd) -1)
           (mms dt val dt val -1)))

        ((< val min-val)
         (mms dt val (mms-max-d state) max-val mode))

        ((> val max-val)
         (mms (mms-min-d state) min-val dt val mode))

        (else
         state))))

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
  (require "private/test-support.rkt")
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

