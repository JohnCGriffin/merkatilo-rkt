#lang racket/base

;; Given a symbol or string like 'IBM or 'IBM::VOLUME, return a series.
;; The action taken is the function referenced by the parameter
;; current-series-loader.  The default value of current-series-loader
;; is default-loader, which loads from plain text files in ~/TIME_SERIES.
;; Generally, one will be using a data service with knowledge about
;; access and decoding outside this library's scope.


(require
 (combine-in
  (only-in racket/contract contract-out -> case->)
  "private/contracts.rkt"
  "core/series.rkt"
  "private/default-loader.rkt"
  "rename-series.rkt"))

(provide
 (contract-out
  [ current-series-loader (case-> (-> (-> series-name? series?) void?)
                                  (-> (-> series-name? series?)))]
  [ lo (-> series-name? series?)]))


(define current-series-loader (make-parameter default-loader))

(define (lo original-id)
  (define (normalize-load-id original-id)
    (define string-id (format "~a" original-id))
    (if (or (regexp-match #px"::" string-id)
            (regexp-match #px"/" string-id))
        string-id
        (format "~a::CLOSE" string-id)))
  (define id (normalize-load-id original-id))
  (define loader (current-series-loader))
  (rename-series (loader id) original-id))



