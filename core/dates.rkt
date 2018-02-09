#lang racket/base

;; The dateset structure is the mechanism to define an ordered collection
;; of jdate.  This implementation is an immutable vector of jdate.  Most
;; operations iterate over a dateset.  Usually that dateset is defaulted
;; to current-dates, a thread-local parameter.

;; Constructing a dateset is usually done via the 'dates' operator which
;; allows for union or intersection of multiple sources.  A common input
;; is a series as in (dates IBM) or (dates IBM #:first (today -100)).

;; Typical iteration looks:

;; (for ((dt (in-vector (dateset-vector some-dates))))
;;   ...)

;; In convenience.rkt is a in-dates macro that simplifies access, but is
;; not used in the library because erasing the in-vector form skips
;; considerable performance enhancements for vectors inside for/ loops.
;; In short, use the in-dates macro as a client of this library, but
;; in-vector within the library.


(require (only-in racket/function curry identity)
         (only-in racket/set set? set-member? for/set set->list)
         (only-in racket/contract -> ->* contract-out listof case-> or/c flat-named-contract)
         "jdate.rkt"
         "series.rkt"
         "../private/binary-search.rkt")


(define dates-convertible/c
  (flat-named-contract
   'dates-convertible/c
   (λ (item)
     (or (dateset? item)
	 (series? item)
	 (date-range? item)
	 (set? item)))))

(provide (except-out (struct-out dateset) dateset)

         with-dates

         set-dates

         (struct-out date-range)

         (contract-out

          [ ->dateset (-> dates-convertible/c  dateset?) ]

          [ dateset->set (-> dateset? set?) ]

          [ date-range-predicate (-> date-range? (-> jdate? boolean?)) ]

          [ dates-contiguous? (-> dateset? boolean?) ]

          [ current-dates (case-> (-> dateset? void?)
                                  (-> dateset?)) ]

          [ nearest (->* ()
                         (jdate?
                          #:dates dateset?
                          #:next boolean?)
                         jdate?)]
          
          [ nearest+ (->* ()
                          (jdate?
                           #:dates dateset?)
                          jdate?)]

          [ first-date (->* () (dateset?) jdate?)]
          [ last-date (->* () (dateset?) jdate?)]
          
          [ dates (->* ()
                       (#:first (or/c symbol? jdate?)
                        #:last (or/c symbol? jdate?)
                        #:expanded boolean?
                        #:union boolean?)
                       #:rest (listof dates-convertible/c)
                       dateset?)]))


; Scoped dates
(define-syntax-rule (with-dates SPEC body ...)
  (parameterize ((current-dates (->dateset SPEC)))
    body ...))


; Set the current dates parameter
(define-syntax-rule (set-dates arg ...)
  (let ((dts (dates arg ...)))
    (current-dates dts)))

;----------- The main feature -----------------

(define locally-constructed (make-parameter #f))

(struct dateset (vector)

  #:guard (λ (v name)

            (define problem
              (cond
                ((not (vector? v)) "not a vector")
                ((zero? (vector-length v)) "zero-length vector")
                ((not (jdate? (vector-ref v 0))) "vector contains non-jdate")
                ((not (locally-constructed))
                 (for/first ((d0 (in-vector v))
                             (d1 (in-vector v 1))
                             #:when (or (not (jdate? d1))
                                        (<= d1 d0)))
                   (if (jdate? d1)
                       "unordered dates"
                       "vector contains non-jdate")))
                (else #f)))

            (if problem
                (raise-user-error 'dateset problem)
                (values (vector->immutable-vector v))))

  #:methods gen:custom-write
  [(define write-proc
     (λ (instance port mode)
       (fprintf port "<dateset:~a..~a>"
                (jdate->text (first-date instance))
                (jdate->text (last-date instance)))))])




; a dateset is created from union or intersection
; of series, other datesets, arity-1 procedures and sets
(define (dates #:first (first MIN-DATE)
               #:last  (last MAX-DATE)
               #:union (union #f)
               #:expanded (expanded #f)
               . specification-items)

  (define earliest-possible
    (let ((dts (map (λ (spec)
                      (cond
                        ((and (date-range? spec) (date-range-first spec))
                         (date-range-first spec))
                        ((dated-series? spec)
                         (dated-series-first-date spec))
                        ((dateset? spec)
                         (vector-ref (dateset-vector spec) 0))
                        (else
                         MIN-DATE))) specification-items)))
      (and (pair? dts)
           (apply min dts))))
  
  (define latest-possible
    (let ((dts (map (λ (spec)
                      (cond
                        ((and (date-range? spec) (date-range-last spec))
                         (date-range-last spec))
                        ((dated-series? spec)
                         (dated-series-last-date spec))
                        ((dateset? spec)
                         (let ((v (dateset-vector spec)))
                           (vector-ref v (sub1 (vector-length v)))))
                        (else
                         MAX-DATE))) specification-items)))
      (and (pair? dts)
           (apply max dts))))


  (define FD
    (if earliest-possible
        (max earliest-possible (->jdate first))
        (->jdate first)))

  (define LD
    (let ((LD (if latest-possible
                  (min latest-possible (->jdate last))
                  (->jdate last))))
      (if (< LD FD)
          (raise-user-error 'dates "specified date boundaries would create empty dateset")
          LD)))
  

  (define (->predicate X)
    (cond
      ((date-range? X)
       (date-range-predicate X))
      ((series? X)
       (series-function X))
      ((dateset? X)
       (curry set-member? (dateset->set X)))
      ((set? X)
       (curry set-member? X))
      (else
       (raise-user-error 'dates "unsupported type ~a" X))))
  
  (define predicates (map ->predicate specification-items))
  
  (define composite-predicate
    (cond
      ((null? predicates) identity)
      (union (λ (dt)
               (for/or ((p (in-list predicates)))
                 (p dt))))
      (else (λ (dt)
              (for/and ((p (in-list predicates)))
                (p dt))))))

  (define vec
    (let* ((vec (for/vector ((dt (in-range FD (add1 LD) 1))
                             #:when (composite-predicate dt)) dt))
           (vec-len (vector-length vec)))
      (if (and expanded (> vec-len 1))
          (let ((start (vector-ref vec 0))
                (end (add1 (vector-ref vec (sub1 vec-len)))))
            (for/vector ((dt (in-range start end))) dt))
          vec)))

  (parameterize ((locally-constructed #t))
    (dateset vec)))

(define (->dateset item)
  (if (dateset? item)
      item
      (dates item)))


;-------- dateset operators ------------


(define (nearest (dt (today))
                 #:dates (dts (current-dates))
                 #:next (next #f))

  (unless (<= (first-date dts) dt (last-date dts))
    (raise-user-error 'nearest "~a out of dates bounds" (jdate->text dt)))

  (define dv (dateset-vector dts))
  (define search-result (binary-search-or-insertion dv dt))

  (cond
    ((integer? search-result)
     (vector-ref dv search-result))
    (next
     (vector-ref dv (insertion-point search-result)))
    (else
     (vector-ref dv (sub1 (insertion-point search-result))))))

(define (nearest+ (dt (today))
                  #:dates (dts (current-dates)))
  (nearest dt #:dates dts #:next #t))


(define (first-date (dates (current-dates)))
  (define dv (dateset-vector dates))
  (vector-ref dv 0))

(define (last-date (dates (current-dates)))
  (define dv (dateset-vector dates))
  (vector-ref dv (-(vector-length dv) 1)))

(define current-dates
  (make-parameter
   (dates #:first MIN-DATE #:last MIN-DATE)
   (λ (dts)
     (if (dateset? dts)
         dts
         (raise-user-error 'current-dates "requires dateset, date-range or series")))))

(define (dates-contiguous? dts)
  (define fd (first-date dts))
  (define ld (last-date dts))
  (define days (add1 (- ld fd)))
  (eqv? days (vector-length (dateset-vector dts))))

(define (dateset->set dts)
  (define dv (dateset-vector dts))
  (for/set ((dt (in-vector dv))) dt))

(struct date-range (first last)
  #:guard (λ (first last name)
            (let ((fd (->jdate first #:check? #t))
                  (ld (->jdate last #:check? #t)))
              (when (and fd ld (< ld fd))
                (raise-user-error 'date-range "unordered dates"))
              (values fd ld)))

  #:methods gen:custom-write
  [(define write-proc
     (λ (instance port mode)
       (let ((fd (date-range-first instance))
             (ld (date-range-last instance)))
         (fprintf port "<~a ~a>"
                  (and fd (jdate->text fd))
                  (and ld (jdate->text ld))))))])


(define (date-range-predicate instance)
  (let ((fd (date-range-first instance))
        (ld (date-range-last instance)))
    (λ (query-date)
      (cond
        ((and fd (> fd query-date)) #f)
        ((and ld (< ld query-date)) #f)
        (else #t)))))






;============================================================

(module+ test
  (require rackunit)

  (check-false (let ((dts (dates #:first '2001-1-1 #:last '2010-1-1)))
                 (for/first ((date-1 (in-vector (dateset-vector dts) 1))
                             (date-0 (in-vector (dateset-vector dts) 0))
                             #:when (not (eqv? (- date-1 date-0) 1)))
                   'oops)))

  (check-exn
   exn:fail?
   (λ ()
     (dates #:first '2000-1-1 #:last '1999-1-1)))

  (check-not-exn
   (λ ()
     (dates #:first '2000-1-1 #:last '2001-1-1)))

  (check-true
   (let ((dts-2000 (dates #:first '2000-1-1 #:last '2001-1-1))
         (dts-2001 (dates #:first '2001-1-1 #:last '2002-1-1)))
     (parameterize ((current-dates dts-2000))
       (and (eq? dts-2000 (current-dates))
            (eq? dts-2001 (parameterize ((current-dates dts-2001)) (current-dates)))
            (eq? dts-2000 (current-dates))))))

  (let ((predicate (date-range-predicate (date-range '2002-1-1 '2004-1-1))))
    (check-equal? (map predicate (map ->jdate '(2000-1-1 2002-1-1 2003-1-1 2004-1-1 2005-1-1)))
                  (list #f #t #t #t #f)))
  
  (check-exn
   exn:fail?
   (λ ()
     (date-range '2001-1-1 '2000-1-1)))

  (check-exn
   exn:fail?
   (λ ()
     (current-dates 'hello)))

  (check-not-exn
   (λ ()
     (current-dates (dates #:first '2001-1-1 #:last '2002-3-3)))))

