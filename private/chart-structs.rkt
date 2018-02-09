#lang racket/base

(require racket/list
         racket/match)

(provide ifhash
         make-padding
         padding-t+b
         padding-l+r
         (struct-out ∝)
         (struct-out padding)
         (struct-out component)
         (struct-out plot-component)
         (struct-out sigplot-component)
         (struct-out label-component)
         (struct-out container)
         (struct-out chart-container)
         (struct-out clip-container))


(struct ∝ (value))

(define (ifhash attr-pairs)
  (for/hash ((entry (in-list attr-pairs))
             #:when (second entry))
    (match entry
      [ (list k v p) #:when (p v) (values k v) ]
      [ (list k _ _) (raise-user-error 'ifhash "bad type for ~a" k) ]
      [ (list k v) (values k v) ]
      [ else (raise-user-error "bad call to (ifhash ..)")])))

(struct padding (top right bottom left)  #:transparent)
(define (make-padding #:top    (top    0)
                      #:right  (right  0)
                      #:left   (left   0)
                      #:bottom (bottom 0))
  (padding top right bottom left))

(define (padding-l+r p)
  (+ (padding-right p)
     (padding-left p)))

(define (padding-t+b p)
  (+ (padding-top p)
     (padding-bottom p)))


(struct component (attrs)
  #:transparent
  #:guard (λ (attrs name)
            (when (list? attrs)
              (set! attrs (ifhash attrs)))
            (unless (hash? attrs)
              (raise-user-error
               (format "~a attrs must be dictionary" name)))
            (values attrs)))

(struct plot-component component (series))

(struct sigplot-component component (series signals))

(struct label-component component (text)
  #:transparent
  #:guard (λ (a t name)
            (unless (string? t)
              (raise-user-error
               (format "~a given non-string ~a" name t)))
            (values a t)))


(struct container component (children)
  #:guard (λ (attrs children name)
            (unless (and (list? children)
                         (andmap component? children))
              (raise-user-error
               (format "~a children must be list of components"
                       name)))
            (values attrs children)))

(struct clip-container container ())

(struct chart-container container ())

