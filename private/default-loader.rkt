#lang racket/base


(require (only-in racket/string string-replace)
         (only-in racket/contract contract-out ->)
         (only-in racket/promise force delay)
         (only-in net/url get-pure-port string->url)
         (only-in json read-json)
         "../private/contracts.rkt"
         "../core/series.rkt"
         "../serialize.rkt")

(provide
 (contract-out
  [ default-loader (-> series-name? series?)]))

(define loader-config-promise
  (delay 
    (call-with-input-file "/etc/merkatilo/default-loader-config.json" read-json)))


(define (worker id)

  (define data-source (hash-ref (force loader-config-promise) 'data-source))
  (define data-source-regex (pregexp (hash-ref data-source 'regex)))
  (define data-source-replacement (hash-ref data-source 'replacement))

  (define the-url
    (string->url
     (regexp-replace data-source-regex id data-source-replacement)))

  (serialize-in
   (get-pure-port the-url)))

(struct expiring-item (expiration item))

(define cache (hash))

(define (next-even-time-frame)
  (define cache-seconds (hash-ref (force loader-config-promise) 'cache-seconds 3600))
  (define recent-time-frame (* cache-seconds (quotient (current-seconds) cache-seconds)))
  (+ cache-seconds recent-time-frame))

(define (default-loader id)
  (define result (hash-ref cache id #f))
  (if (and result
           (> (expiring-item-expiration result)
              (current-seconds)))
      (expiring-item-item result)
      (let* ((calculated (worker id))
             (expiration (next-even-time-frame))
             (entry (expiring-item expiration calculated)))
        (set! cache (hash-set cache id entry))
        calculated)))

