#lang racket/base

(provide average
         fontkodo-mean
         standard-deviation
	 signalify-vector-copy)


(require (only-in racket/vector 
                  vector-map
                  vector-copy))

(require (rename-in racket/unsafe/ops [ unsafe-fl* fl* ]))


; Signals are either negative or non-negative
(define (signalify-vector-copy v)
  (define copy (make-vector (vector-length v) #f))
  (for/fold ((prev #f))
            ((val (in-vector v))
             (ndx (in-naturals))
             #:when val)
    (define sig (if (< val 0) -1 1))
    (when (not (eqv? sig prev))
      (vector-set! copy ndx sig))
    sig)
  copy)


(define (fontkodo-mean e nums)
  (cond
    ((vector? nums)
     (fontkodo-mean e (vector->list nums)))
    ((zero? e)
     (expt (apply * nums)
           (/ (length nums))))
    (else
     (expt (/ (for/sum ((n (in-list nums)))
                (expt n e))
              (length nums))
           (/ e)))))

(define (average vec)
  (define-values (N total)
    (for/fold ((N 0)
               (total 0))
              ((n (in-vector vec)) #:when n)
      (values (add1 N)
              (+ total n))))
  (if (zero? N)
      #f
      (/ total N)))


(define (standard-deviation vec)
  (define mean
    (let ((m (average vec)))
      (and m (real->double-flonum m))))
  (define avg-squared-diff
    (and mean
         (average
          (vector-map
           (lambda (n)
             (define diff (and n (- n mean)))
             (and diff (fl* diff diff)))
           vec))))
  (and avg-squared-diff
       (sqrt avg-squared-diff)))
