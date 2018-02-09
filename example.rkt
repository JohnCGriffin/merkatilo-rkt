#lang racket/base

(require merkatilo
         merkatilo/private/test-support)

(set-dates TEST-SERIES)

(define sigs (to-signals (mo TEST-SERIES 5)))

(dump (conviction sigs 4))

