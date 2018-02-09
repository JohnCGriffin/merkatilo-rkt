#lang racket/base

(provide with-dc-state)

(require racket/class)

(define-syntax with-dc-state
  (syntax-rules ()
    [(with-dc-scope the-dc body ...)
     (let ((pre-font (send the-dc get-font))
           (pre-brush (send the-dc get-brush))
           (pre-pen (send the-dc get-pen))
           (pre-tfg (send the-dc get-text-foreground))
           (pre-xfr (send the-dc get-transformation)))
       (let ()
         body ...)
         (send* the-dc
           (set-font pre-font)
           (set-brush pre-brush)
           (set-pen pre-pen)
           (set-text-foreground pre-tfg)
           (set-transformation pre-xfr)))]))
