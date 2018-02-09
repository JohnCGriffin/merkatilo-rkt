#lang racket/base

(require (only-in racket/contract contract-out -> any/c)
         "../core/series.rkt")

(provide
 (contract-out
  [ abbreviate (-> any/c string?) ]))


(define (abbreviate item)
  (define text (format "~a" (if (series? item)
                                (series-name item)
                                item)))
  (if (> (string-length text) 30)
      "..."
      text))

