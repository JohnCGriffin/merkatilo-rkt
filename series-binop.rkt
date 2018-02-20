#lang racket/base

(require
 (combine-in
  "private/common-requirements.rkt"
  "series-map.rkt"
  "constant.rkt"
  (only-in "private/contracts.rkt" binop/c)))


(provide
 (contract-out
  [ add binop/c ]
  [ sub binop/c ]
  [ div binop/c ]
  [ mul binop/c ]
  [ lt binop/c ]
  [ le binop/c ]
  [ gt binop/c ]
  [ ge binop/c ]))

(define-syntax-rule (define-binop name op)
  (define (name a b)
    (series-math (quote name) op a b)))

(define (series-math label proc a b)
  (define (serify item)
    (if (number? item)
        (constant item)
        item))
  (rename-series
   (series-map proc
               (serify a)
               (serify b))
   (format "(~a ~a ~a)" label (abbreviate a) (abbreviate b))))

(define-binop add +)
(define-binop sub -)
(define-binop mul *)
(define-binop div (λ (n d)
                    (and (not (zero? d))
                         (/ n d))))


(define (inequality f2)
  (λ (n0 n1)
    (and (f2 n0 n1) n0)))

(define-binop lt (inequality <))
(define-binop le (inequality <=))
(define-binop gt (inequality >))
(define-binop ge (inequality >=))



;===========================================

(module+ main 
  (require "private/test-support.rkt")
  (typical-run (λ () (add TEST-SERIES TEST-SERIES))
               (λ () (add 123 TEST-SERIES))
               (λ () (div TEST-SERIES 2))
               (λ () (mul TEST-SERIES 3))))


;===========================================

(module+ test
  (require rackunit
           "series-count.rkt"
           "private/test-support.rkt")

  (with-dates TEST-SERIES
    
    (check-equal?
     (series-count (lt TEST-SERIES 300))
     214)

    (check-equal?
     (series-count (ge TEST-SERIES 300))
     540)

    (for ((n (in-range 200 400 20)))
      (check-equal?
       (+ (series-count (lt TEST-SERIES n))
          (series-count (ge TEST-SERIES n)))
       (series-count TEST-SERIES))
      (check-equal?
       (+ (series-count (le TEST-SERIES n))
          (series-count (gt TEST-SERIES n)))
       (series-count TEST-SERIES)))

    (check-not-exn
     (λ ()
       (with-dates TEST-SERIES
         (verify-equivalency
          (mul 3 TEST-SERIES)
          (series-map + TEST-SERIES TEST-SERIES TEST-SERIES)))))

    (check-not-exn
     (λ ()
       (with-dates TEST-SERIES
         (verify-equivalency
          (div (add TEST-SERIES TEST-SERIES) 2)
          TEST-SERIES))))

    (check-not-exn
     (λ ()
       (with-dates TEST-SERIES
         (verify-equivalency
          (mul 2 TEST-SERIES)
          (add TEST-SERIES TEST-SERIES)))))))






