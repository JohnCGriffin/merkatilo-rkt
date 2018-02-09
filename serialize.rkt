#lang racket/base


(require (only-in racket/port port->lines)
         (only-in racket/string string-split)
         (only-in racket/list filter-map first second)
         (only-in racket/contract contract-out ->)
         "core/dates.rkt"
         "core/jdate.rkt"
         "core/series.rkt"
         "obs-series.rkt")

(provide
 (contract-out
  [ serialize-out (-> series? dateset? port? void?) ]
  [ serialize-in (-> input-port? series?) ]))


(define (serialize-out s dts out-port)
  (define sf (series-function s))
  (for ((dt (in-vector (dateset-vector dts))))
    (define val (sf dt))
    (when val
      (fprintf out-port "~a ~a\n" (jdate->text dt)
	       (if (integer? val)
		   (inexact->exact val)
		   val)))))


(define (serialize-in in-port)
  (define (parse-line ln)
    (define tokens (string-split ln))
    (define dt (and (eqv? 2 (length tokens))
                    (->jdate (first tokens) #:check? #t)))
    (define val (and dt
                     (exact->inexact (string->number (second tokens)))))
    (and val (observation dt val)))
  (define lines (port->lines in-port))
  (define obs (filter-map parse-line lines))
  (obs->series obs))



