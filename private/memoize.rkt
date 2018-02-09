#lang racket/base

(require (only-in racket/contract contract-out -> case->))

(provide
 #;without-weak-memoization
 (contract-out
  #;[ weak-memoization (case-> (-> boolean? void?)
                             (-> boolean?)) ]
  [ memoize (-> procedure? procedure?) ]
  #;[ weak-memoize (-> procedure? procedure?) ]))

(define (memoize f)
  (let ((lookup (hash)))
    (lambda ARGS
      (or (hash-ref lookup ARGS #f)
          (let ((result (apply f ARGS)))
            (set! lookup (hash-set lookup ARGS result))
            result)))))

(define weak-memoization (make-parameter #t))

(define-syntax-rule (without-weak-memoization body ...)
  (parameterize ([weak-memoization #f])
    body ...))

(define function-cache
  (let ((cache '()))
    (lambda (f ARGS)
      (define active-cache 
        (if (zero? (random 100)) 
            (filter weak-box-value cache)
            cache))
      (define search-result
        (if (weak-memoization)
            (let loop ((cache active-cache))
              (let ((content (and (pair? cache)
                                  (weak-box-value (car cache)))))
                (cond
                  ((null? cache) #f)
                  ((and content
                        (equal? ARGS (cddr content)))
                   (car content))
                  (else
                   (loop (cdr cache))))))
            (apply f ARGS)))
      (or search-result
	  (let* ((result (apply f ARGS))
                 (new-entry (cons result (cons f ARGS)))
                 (wbox (make-weak-box new-entry)))
	    (set! cache (cons wbox active-cache))
	    result)))))

(define (weak-memoize f)
  (λ ARGS
    (function-cache f ARGS)))

