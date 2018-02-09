#lang racket/base

(require (only-in racket/function identity)
         (only-in racket/string string-join)
         "private/common-requirements.rkt")

(provide
 (contract-out
  [ dump (->* () (#:first datespec? #:last datespec? #:dates DS) #:rest (listof S) void?)]))


(define (dump #:first (first '1800-1-1)
              #:last  (last '2100-1-1)
              #:dates (dts (current-dates))
              . series-specifications)

  (define bunch
    (for/list ((s (in-list series-specifications)))
      (series-function s)))

  (define date-predicate
    (let ((fd (->jdate first))
          (ld (->jdate last)))
      (λ (dt)
        (<= fd dt ld))))

  (define (formatted num)
    (define TWELVE-SPACES "            ")
    (define (numfmt n)
      (define s (string-append TWELVE-SPACES (real->decimal-string n 4)))
      (substring s (- (string-length s) 12)))  
    (if num (numfmt num) TWELVE-SPACES))


  (for ((dt (in-vector (dateset-vector dts)))
        #:when (date-predicate dt))
    (define snag (λ (s) (s dt)))
    (define cols (map snag bunch))
    (when (ormap identity cols)
      (printf "~a  ~a\n"
              (jdate->text dt)
              (string-join (map formatted cols))))))



