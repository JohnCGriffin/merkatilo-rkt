#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [ insertion? (-> any/c boolean?) ]
  [ insertion-point (-> insertion? (and/c integer? (>=/c 0))) ]
  [ binary-search-or-insertion (->* (vector? any/c)
                                    (#:less-than (-> any/c any/c boolean?))
                                    (or/c (and/c integer? (>=/c 0))
                                          insertion?))]))

(struct insertion (point) #:transparent)

; return the index of the found query
; or give the insertion point.
(define (binary-search-or-insertion vec query #:less-than (lt <))

  (define (comparator a b)
    (cond
      ((lt a b) -1)
      ((lt b a) 1)
      (else 0)))

  (define (search-between l h)
    (define m (quotient (+ l h) 2))
    (define l-val (vector-ref vec l))
    (define h-val (vector-ref vec h))
    (define m-val (vector-ref vec m))
    (define cmp (comparator m-val query))
    (cond
      ((zero? cmp)
       m)
      ((eqv? (- h l) 1)
       (if (zero? (comparator h-val query)) h (insertion h)))
      ((negative? cmp)
       (search-between m h))
      (else
       (search-between l m))))

  (define len (vector-length vec))
  (cond
    ((zero? len) (insertion 0))
    ((lt query (vector-ref vec 0)) (insertion 0))
    ((lt (vector-ref vec (sub1 len)) query) (insertion len))
    (else (search-between 0 (sub1 len)))))



;=============================================

(module+ test

  (require rackunit)

  (define V #(10 20 30 40))
  (define NAMES (list->vector (sort '("Fred" "Betty" "Wilma" "Barney") string<?)))

  (check-equal? (binary-search-or-insertion V (sub1 (vector-ref V 0)))
                (insertion 0))

  (check-equal? (binary-search-or-insertion V 100)
                (insertion (vector-length V)))

  (check-equal?
   (for/list ((ndx (in-range (vector-length V)))) ndx)
   (for/list ((val (in-vector V))) (binary-search-or-insertion V val)))

  (check-equal?
   (for/list ((ndx (in-range (vector-length V)))) (insertion (add1 ndx)))
   (for/list ((val (in-vector V))) (binary-search-or-insertion V (add1 val))))

  (check-equal?
   (for/list ((ndx (in-range (vector-length NAMES)))) (insertion (add1 ndx)))
   (for/list ((name (in-vector NAMES)))
     (define new-name (string-append name "suffix"))
     (binary-search-or-insertion NAMES new-name #:less-than string<?)))

  (check-equal?
   (binary-search-or-insertion NAMES "Fred" #:less-than string<?)
   2)

  (check-equal?
   (binary-search-or-insertion NAMES "Alice" #:less-than string<?)
   (insertion 0)))
