#lang racket/base

(provide (all-defined-out))

(define-syntax-rule (second-of-values body ...)
  (cadr (call-with-values (λ () body ...) list)))

(define-syntax-rule (first-of-values body ...)
  (car (call-with-values (λ () body ...) list)))

