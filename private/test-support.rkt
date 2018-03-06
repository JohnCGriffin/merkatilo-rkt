#lang racket/base

;; The TEST-SERIES is just a sum of IBM+SPY for 3 years,
;; rounded for ease of manual checknig of algorithms.


(provide TEST-SERIES
         EMA-3-SERIES
         MO-3-SERIES
         SMA-3-SERIES
         REVERSALS-95-105-SERIES
         REVERSALS-91-109-SERIES
         CROSS-EMA-30-SERIES
         MO-5-CONVICTION-4-SERIES
         EQUITYLINE-EMA-10
         typical-run
         verify-equivalency
         literal-series
         approx
         dump
         NEVER-SERIES)

; reexport some handies
(provide with-dates
         first-date
         last-date
         today
         dates
         current-dates
         ->jdate
         jdate->text
         series-count
         ob-d ob-v
         dateset-vector
         (struct-out date-range)
         (struct-out series))

(require racket/list
         racket/file
         racket/string
         "memoize.rkt"
         "series-dates-values.rkt"
         "../core/jdate.rkt"
         "../core/series.rkt"
         "../core/dates.rkt"
         "../obs-series.rkt"
         "../series-count.rkt"
         "../dump.rkt")

(define (approx num)
  (define big 1000000000)
  (/ (round (* big num)) big))


(define (literal-series date-vals)
  (obs->series (map (λ (p)
                      (define dt (->jdate (car p)))
                      (define val (cadr p))
                      (observation dt val)) date-vals)))

(define (typical-run #:iterations (iterations 1000)
                     #:full-dump (full-dump #f)
                     . thunks)
  
  (define result
    (with-dates FULL-DATES
      (map (λ (t) (t)) thunks)))

  (printf "dump of\n")
  (for ((s (in-list result)))
    (printf "\t~a\n" (series-name s)))
  
  (if full-dump
      (with-dates FULL-DATES
        (apply dump result))
      (begin
        (with-dates BEGINNING-DATES
          (apply dump result))
        (printf "...\n")
        (with-dates RECENT-DATES
          (apply dump result))))

  ; Force lazy series via series-dates-values
  (with-dates TEST-SERIES
    (define F (last thunks))
    (define dv (dateset-vector (current-dates)))
    (collect-garbage)
    (collect-garbage)
    (printf "\n~a iterations: " iterations)
    (time
     (for ((i (in-range iterations)))
       (series-dates-values (F) dv)))))



(require "../serialize.rkt")

(define (txt-file-series name)
  (with-input-from-file (format "/tmp/merkatilo-test-data/~a.txt" name)
    (λ () (serialize-in (current-input-port)))))

(define TEST-SERIES (txt-file-series "test-series"))
(define EMA-3-SERIES (txt-file-series "ema-3"))
(define SMA-3-SERIES (txt-file-series "sma-3"))
(define MO-3-SERIES (txt-file-series "mo-3"))
(define REVERSALS-95-105-SERIES (txt-file-series "reversals-95-105"))
(define REVERSALS-91-109-SERIES (txt-file-series "reversals-91-109"))
(define CROSS-EMA-30-SERIES (txt-file-series "cross-ema-30"))
(define MO-5-CONVICTION-4-SERIES (txt-file-series "mo-5-conviction-4"))
(define EQUITYLINE-EMA-10 (txt-file-series "equityline-ema-10"))

#;(define TEST-SERIES
  (let* ((lines (file->lines "/tmp/merkatilo-test-data/test-series.txt"))
         (split-lines (map string-split lines))
         (obs (map (λ (sl)
                     (and (eqv? 2 (length sl))
                          (regexp-match #px"^[12]" (car sl))
                          (observation (->jdate (car sl))
                                       (string->number (cadr sl)))))
                   split-lines)))
    (obs->series
     #:name "TEST-SERIES"
     obs)))

(define FULL-DATES
  (dates TEST-SERIES (date-range '2015-1-1 '2015-1-5) #:union #t))

(define BEGINNING-DATES
  (dates FULL-DATES #:last '2012-1-15 #:expanded #t))

(define RECENT-DATES
  (dates FULL-DATES #:first '2014-12-10 #:expanded #t))

(define NEVER-SERIES
  (series (λ (dt) #f) "never more"))

(define (verify-equivalency a b)
  (define af (series-function a))
  (define bf (series-function b))
  (for ((dt (in-vector (dateset-vector (current-dates)))))
    (define a-val (af dt))
    (define b-val (bf dt))
    (unless (eq? (not a-val)
                 (not b-val))
      (raise-user-error 'verify-equivalency
                        "at ~a, ~a exists but ~a does not"
                        (jdate->text dt)
                        (if a-val "A" "B")
                        (if a-val "B" "A")))
    (unless (or (not a-val)
                (< (abs (- a-val b-val)) 0.00001))
      (raise-user-error 'verify-equivalency "at ~a, A = ~a, B = ~a"
                        (jdate->text dt) a-val b-val))))


