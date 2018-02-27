#lang racket/base


(require (only-in racket/string string-replace)
         (only-in racket/contract contract-out ->)
         "../private/contracts.rkt"
         "../core/series.rkt"
         "../serialize.rkt")

(provide
 (contract-out
  [ default-loader (-> series-name? series?)]))

(define (worker id)
  
  (define (id->filename id)
    (if (regexp-match #px"^/+" id)
        id
        (format "~a/TIME_SERIES/~a"
                (getenv "HOME")
                (string-replace id #px":+" "/"))))

  (call-with-input-file (id->filename id) serialize-in))


(struct expiring-item (expiration item))

(define cache (hash))

(define (next-even-hour)
  (define top-of-recent-hour (* 3600 (quotient (current-seconds) 3600)))
  (+ 3600 top-of-recent-hour))

(define (default-loader id)
  (define result (hash-ref cache id #f))
  (if (and result
           (> (expiring-item-expiration result)
              (current-seconds)))
      (expiring-item-item result)
      (let* ((calculated (worker id))
             (expiration (next-even-hour))
             (entry (expiring-item expiration calculated)))
        (set! cache (hash-set cache id entry))
        calculated)))

