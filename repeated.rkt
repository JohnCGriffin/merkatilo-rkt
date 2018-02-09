#lang racket/base

(require "private/common-requirements.rkt")

(provide
 (contract-out
  [ repeated (->* (S) (#:repeat-last boolean? #:dates DS) S) ]))


(define (repeated s #:dates (dts (current-dates)) #:repeat-last (repeat-last #f))
  
  (define dv (dateset-vector dts))
  
  (define vv (series-dates-values s dv))
  
  (define-values (fd out-v)
    (dates-appropriate-fd-and-vec dts))
  
  (define last-valid-slot
    (for/fold ((last-valid-slot #f))
	((dt (in-vector dv))
	 (val (in-vector vv))
	 #:when val)
      (define slot (- dt fd))
      (vector-set! out-v slot val)
      slot))
  

  (define stop-slot
    (and last-valid-slot
	 (if repeat-last (vector-length out-v) last-valid-slot)))

  (when stop-slot
    (for/fold ((last-val #f))
	((ndx (in-range 0 stop-slot))
	 (val (in-vector out-v)))
      (define out-val (or val last-val))
      (when (and (not val)
		 out-val)
	(vector-set! out-v ndx out-val))
      out-val))

  (make-vector-series
   #:name (format "(repeated ~a)" (abbreviate s))
   #:vector out-v
   #:first-date fd))



;============================================

(module+ test
  (require rackunit
           "series-count.rkt"
           "private/test-support.rkt")
  
  (with-dates TEST-SERIES    
    (check-equal? (series-count TEST-SERIES) 754)
    (check-equal?
     (series-count TEST-SERIES)
     (series-count (repeated TEST-SERIES))))

  (with-dates (dates #:first '2013-1-3 #:last '2020-1-1)
    (check-equal?
     (series-count (repeated TEST-SERIES #:repeat-last #t))
     (vector-length (dateset-vector (current-dates))))))



