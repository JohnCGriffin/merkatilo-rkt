#lang racket/base

(provide (rename-out [one-pass-standard-deviation standard-deviation])
	 signalify-vector-copy)


(require
 (only-in racket/unsafe/ops unsafe-fl*))


; Signals are either negative or non-negative
(define (signalify-vector-copy v)
  (define result (make-vector (vector-length v) #f))
  (for/fold ((prev #f))
            ((val (in-vector v))
             (ndx (in-naturals))
             #:when val)
    (define sig (if (< val 0) -1 1))
    (when (not (eqv? sig prev))
      (vector-set! result ndx sig))
    sig)
  result)



(define (one-pass-standard-deviation vec)
  (for/fold ((N 0)
             (total 0.0)
             (total² 0.0)
             #:result (and (not (zero? total²))
                           (sqrt (/ (- total² (/ (* total total) N)) N))))
            ((n (in-vector vec))
             #:when n)
    (values (add1 N)
            (+ n total)
            (+ (* n n) total²))))


#;(define (standard-deviation vec)

  (define-values (N total)
    (for/fold ((N 0)
               (total 0.0))
              ((n (in-vector vec)) #:when n)
      (values (add1 N)
              (+ total n))))

  (define mean (and (positive? N)
                    (real->double-flonum (/ total N))))
  
  (define avg-squared-diff
    (and mean
         (/ (for/sum ((n (in-vector vec))
                      #:when n)
              (define diff (- n mean))
              (unsafe-fl* diff diff))
            N)))
  
  (and avg-squared-diff
       (sqrt avg-squared-diff)))
