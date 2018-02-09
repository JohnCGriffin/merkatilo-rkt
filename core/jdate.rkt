#lang racket/base

;; A jdate is a julian integer date constrained between
;; MIN-DATE (1700-1-1) and MAX-DATE (2100-12-31).  jdate
;; is used to query a series or in definitions of the structs
;; dateset and date-range.  The value of the integer coincide
;; with PostgreSQL.


(require (only-in racket/contract contract-out -> ->* any/c or/c and/c between/c)
         "../private/memoize.rkt")

(provide
 (except-out (struct-out ymd) ymd)
 (contract-out
  [ MIN-DATE jdate? ]
  [ MAX-DATE jdate? ]
  [ jdate->text (-> jdate? string?) ]
  [ text->jdate (-> string? jdate?) ]
  [ jdate->ymd (-> jdate? ymd?) ]
  [ ->jdate (->* (any/c)
                 (#:check? boolean?)
                 (or/c jdate? #f)) ]
  [ jdate? (-> any/c boolean?) ]
  [ ymd->jdate (-> year? month? month-day? jdate?) ]
  [ weekday (-> jdate? weekday?) ]
  [ jdate-year (-> jdate? year? ) ]
  [ jdate-month (-> jdate? month?) ]
  [ jdate-day (-> jdate? month-day?) ]
  [ jdate-weekday (-> jdate? (and/c integer? (between/c 0 6))) ]
  [ today (->* () (integer?) jdate?) ]))

(define (->jdate item #:check? (check? #f))
  (cond
    ((jdate? item) item)
    ((string? item) (text->jdate item))
    ((symbol? item) (->jdate (symbol->string item)))
    (check? #f)
    (else (raise-user-error "->jdate argument must be date,string,symbol not ~a" item))))

(define (ymd->jdate year month day)
  
  (define (leap-year? y)
    (cond
      ((not (zero? (remainder y 4))) #f)
      ((zero? (remainder y 400)) #t)
      ((zero? (remainder y 100)) #f)
      (else #t)))
  
  (or (case month
        [(1 3 5 7 8 10 12)
         (< 0 day 32)]
        [(4 6 9 11)
         (< 0 day 31)]
        [(2)
         (or (< 0 day 29)
             (and (eqv? day 29)
                  (leap-year? year)))]
        [else
         (raise-user-error 'ymd->date "invalid month ~a outside [1-12]" month)])
      (raise-user-error 'ymd->date "invalid day of month ~a" day))
  
  (define a (quotient (- 14 month) 12))
  (define y (+ year 4800 (- a)))
  (define m (+ month (* 12 a) -3))
  
  (and
   (+ day
      (quotient (+ (* 153 m) 2) 5)
      (* 365 y)
      (quotient y 4)
      (quotient y -100)
      (quotient y 400)
      -32045)))


(struct ymd (year month day)
  #:guard (λ (y m d name)
            (ymd->jdate y m d)
            (values y m d)))

(define (jdate->ymd julian)
  (let* ((JD (inexact->exact (round julian)))
         (L (+ JD 68569))
         (N (quotient (* 4 L) 146097)))
    (let* ((L (- L (quotient (+ (* 146097 N) 3) 4)))
           (I (quotient (* 4000 (+ L 1)) 1461001)))
      (let* ((L (- L (quotient (* 1461 I) 4) -31))
             (J (quotient (* 80 L) 2447))
             (K (- L (quotient (* 2447 J) 80))))
        (let* ((L (quotient J 11))
               (J (+ J 2 (* -12 L)))
               (I (+ (* 100 (- N 49)) I L)))
          (ymd I J K))))))


(define MIN-YEAR 1700)
(define MAX-YEAR 2100)
(define MIN-DATE (ymd->jdate MIN-YEAR 1 1))
(define MAX-DATE (ymd->jdate MAX-YEAR 12 31))

(define (jdate? j)
  (and (integer? j)
       (<= MIN-DATE j MAX-DATE)))

(define (weekday dt)
  (remainder (add1 dt) 7))

(define (jdate-year dt)
  (ymd-year (jdate->ymd dt)))

(define (jdate-month dt)
  (ymd-month (jdate->ymd dt)))

(define (jdate-day dt)
  (ymd-day (jdate->ymd dt)))

; jdate-weekday -> 0-7 (Sunday - Sat)
(define (jdate-weekday dt)
  (remainder (add1 dt) 7))

(define jdate->text
  (memoize
   (λ (dt)             
     (define (pad num)
       (if (> num 9) num (format "0~a" num)))
     (define X (jdate->ymd dt))
     (format "~a-~a-~a"
             (ymd-year X)
             (pad (ymd-month X))
             (pad (ymd-day X))))))


(define text->jdate
  (memoize
   (λ (date-string)
     (define CHAR-ZERO (char->integer #\0))
     (define CHAR-NINE (char->integer #\9))
     (define (digit c)
       (define val (char->integer c))
       (and (<= CHAR-ZERO val CHAR-NINE)
            (- val CHAR-ZERO)))
     
     (let loop ((digits (string->list date-string))
                (y 0)
                (m 0)
                (d 0)
                (state 0))
       (if (null? digits)
           (ymd->jdate y m d)
           (let ((val (digit (car digits))))
             (cond
               ((not val)
                (loop (cdr digits) y m d (+ state 1)))
               ((eqv? state 2)
                (loop (cdr digits) y m (+ val (* d 10)) state))
               ((eqv? state 0)
                (loop (cdr digits) (+ val (* y 10)) m d state))
               ((eqv? state 1)
                (loop (cdr digits) y (+ val (* m 10)) d state)))))))))

(define (today (days-offset 0))
  (define now (seconds->date (current-seconds)))
  (define y (date-year now))
  (define m (date-month now))
  (define d (date-day now))
  (+ days-offset (ymd->jdate y m d)))


(define (year? y)
  (and (integer? y)
       (<= MIN-YEAR y MAX-YEAR)))

(define (month? m)
  (and (integer? m)
       (<= 1 m 12)))

(define (month-day? d)
  (and (integer? d)
       (<= 1 d 31)))

(define (weekday? w)
  (and (integer? w)
       (<= 0 w 6)))








;=====================================================

(module+ test
  (require rackunit
           racket/set)

  (check-equal? (jdate->text (->jdate '2000-3-3)) "2000-03-03")

  (check-equal? (+ 1 (->jdate '2012-12-31)) (->jdate '2013-1-1))

  (check-equal? (- (->jdate '2000-1-1)
                   (->jdate '1999-1-1)) 365)

  (check-equal? (- (->jdate '2005-1-1)
                   (->jdate '2004-1-1)) 366)

  (check-equal? (let ((new-millenium (->jdate '2000-1-1)))
                  (for/list ((i 8))
                    (jdate-weekday (+ new-millenium i))))
                '(6 0 1 2 3 4 5 6)); 2000-1-1 was Saturday

  (check-exn  exn:fail? (λ () (ymd->jdate 2003 2 29)))

  (check-exn  exn:fail? (λ () (ymd->jdate 12 25 2012))) ; chistmas date American style

  (check-exn  exn:fail? (λ () (ymd->jdate 24 12 2012))) ; christmas in europe

  (check-equal? (jdate->text (ymd->jdate 2004 2 29)) "2004-02-29")

  (check-false (->jdate '() #:check? #t))

  (check-not-false (->jdate '2000-1-1 #:check? #t))
  
  (check-not-false (->jdate '2000-1-1 #:check? #f))

  (check-false (let ((fd (ymd->jdate 1800 1 1))
                     (ld (ymd->jdate 2100 1 1)))
                 (for/first ((dt (in-range fd ld))
                             #:when (not (eqv? dt (text->jdate (jdate->text dt)))))
                   dt)))

  ; 200 years of 365/year + 49 leap years
  (check-equal? (- (ymd->jdate 2090 12 31) (ymd->jdate 1890 12 31))
                (+ (* 365 200) 49)) 

  (check-exn  exn:fail? (λ () (->jdate "not a date")))

  ; 100 dates as string into set should be 100 unique strings
  ; all of string-length 10.
  (check-equal? (let* ((dt (->jdate '2000-1-1))
                       (s1 (for/set ((i (in-range 100)))
                             (jdate->text (+ i dt))))
                       (s2 (for/set ((item (in-set s1)))
                             (string-length item))))
                  (list (set-count s1)
                        (set-count s2)
                        (set-first s2)))
                '(100 1 10)))

