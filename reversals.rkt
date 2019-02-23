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
  (define len (vector-length vv))

  (let loop ((min-d (ob-d init-ob))
             (min-v (ob-v init-ob))
             (max-d (ob-d init-ob))
             (max-v (ob-v init-ob))
             (mode 0)
             (ndx 0))

    (and (< ndx len)
         (let* ((val (vector-ref vv ndx))
                (dt (and val (vector-ref dv ndx))))
           (cond

             ((not val)
              (loop min-d min-v max-d max-v mode (add1 ndx)))
             
             ((and (< mode 1)
                   (> val (* min-v up-factor)))
              (let ((date (if nostradamus min-d dt)))
                (vector-set! out-v (- date fd) 1)
                (loop dt val dt val 1 (add1 ndx))))
             
             ((and (> mode -1)
                   (< val (* max-v down-factor)))
              (let ((date (if nostradamus max-d dt)))
                (vector-set! out-v (- date fd) -1)
                (loop dt val dt val -1 (add1 ndx))))
             
             ((< val min-v)
              (loop dt val max-d max-v mode (add1 ndx)))
             
             ((> val max-v)
              (loop min-d min-v dt val mode (add1 ndx)))
             
             (else
              (loop min-d min-v max-d max-v mode (add1 ndx)))))))


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

