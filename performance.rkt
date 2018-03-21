#lang racket/base

(require (only-in racket/format ~r)
         "equity-line.rkt"
         "series-binop.rkt"
         "volatility.rkt"
         "drawdown.rkt"
         "repeated.rkt"
         "first-last-ob.rkt"
         "private/common-requirements.rkt"
         "series-count.rkt")


(provide
 (struct-out performance)
 (contract-out
  [ performance->text (-> performance? string?) ]
  [ gpa (->* (S) (#:dates DS) optional-real? )]
  [ investment-performance (->* (S)
                                (#:alternate-investment (or/c #f S)
                                 #:signals (or/c #f S)
                                 #:dates DS)
                                performance?)]))


(define (gpa s #:dates (dts (current-dates)))
  (define-values (fo lo)
    (values (first-ob s #:dates dts)
            (last-ob s #:dates dts)))
  (define days (- (ob-d lo) (ob-d fo)))
  (define gain (/ (ob-v lo) (ob-v fo)))
  (define years (/ days 365.2425))
  (- (expt gain (/ 1 years)) 1))


(struct performance (volatility-residual
                     drawdown-residual
                     annualized-gain
                     long-ratio
                     trades) #:transparent)

(define (performance->text instance)
  (format "(vol-res ~a  dd-res ~a  gpa ~a  long ~a  trades ~a)"
          (~r #:precision '(= 3) #:min-width 6 (performance-volatility-residual instance))
          (~r #:precision '(= 3) #:min-width 6 (performance-drawdown-residual instance))
          (~r #:precision '(= 3) #:sign '+ (performance-annualized-gain instance))
          (let ((p (performance-long-ratio instance)))
            (and p (~r #:precision '(= 3) #:min-width 6 p)))                
          (performance-trades instance)))



(define (investment-performance s
                                #:alternate-investment (alternate-investment #f)
                                #:signals (signals #f)
                                #:dates (dts (current-dates)))

  (with-dates dts

    ; The equity line is based upon a real trading day, not the signal from yesterday
    (define equity (if signals
                       (equity-line s signals #:alternate-investment alternate-investment)
                       s))
    
    (define vol-res (- 1.0 (volatility equity #:dates dts)))
    (define dd-res (drawdown-residual equity #:dates dts))
    (define annualized (gpa equity #:dates dts))
    (define-values (long-ratio trades)
      (if signals
          (let ((sig_vals (series-dates-values signals (dateset-vector dts)))
                (trades 0)
                (state #f)
                (longs 0)
                (total 0))
            (for ((sig (in-vector sig_vals)))
              (when sig
                (set! trades (add1 trades))
                (set! state sig))
              (when state
                (set! total (add1 total))
                (when (> state 0)
                  (set! longs (add1 longs)))))
            (values (and (> total 0)
                         (/ longs total))
                    trades))
          (values #f #f)))
    
    (performance vol-res
                 dd-res
                 annualized
                 long-ratio
                 trades)))






;===============================

(module* test racket/base
  
  (require rackunit
           (submod "..")
           "private/test-support.rkt"
           "reversals.rkt")

  (define nostradamus-signals
    (with-dates TEST-SERIES
      (nostradamus TEST-SERIES #:down-factor .97 #:up-factor 1.03)))

  (define nostradamus-performance
    (with-dates TEST-SERIES
      (investment-performance TEST-SERIES #:signals nostradamus-signals)))

  (define nostradamus-text (performance->text nostradamus-performance))

  (with-dates TEST-SERIES
    
    (check-equal?
     (approx (gpa TEST-SERIES))
     (approx 0.07688365986138823))
     
    (check-not-exn
     (λ ()
       (investment-performance TEST-SERIES))))

  (check-equal?
   (approx (gpa TEST-SERIES #:dates (dates TEST-SERIES)))
   (approx 0.07688365986138823))

  (check-equal?
   (approx (performance-volatility-residual nostradamus-performance))
   (approx 0.9278986478200268))

  (check-equal?
   (approx (performance-drawdown-residual nostradamus-performance))
   (approx 0.9712643678160918))

  (check-equal?
   (approx (performance-annualized-gain nostradamus-performance))
   (approx 0.48832261262864174))

  (check-equal?
   (performance-long-ratio nostradamus-performance)
   253/373)

  (check-equal?
   (performance-trades nostradamus-performance) 31))

  



