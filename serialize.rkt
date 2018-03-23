#lang racket/base


(require
 (combine-in
  (only-in racket/port port->lines)
  (only-in racket/string string-split)
  (only-in racket/list filter-map first second)
  (only-in racket/contract contract-out ->)
  "core/dates.rkt"
  "core/jdate.rkt"
  "core/series.rkt"
  "obs-series.rkt"))

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
  (define observation-pattern #px"^\\s*\\d{4}-\\d?\\d-\\d?\\d\\s+-?[0-9\\.]+\\s*$")
  (define (parse-line ln)
    (define parseable (regexp-match observation-pattern ln))
    (define tokens (and parseable (string-split ln)))
    (define dt (and tokens (->jdate (first tokens))))
    (define val (and dt
                     (exact->inexact (string->number (second tokens)))))
    (and val (observation dt val)))
  (define lines (port->lines in-port))
  (define obs (filter-map parse-line lines))
  (obs->series obs))



