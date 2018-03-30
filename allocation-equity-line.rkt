
#lang racket/base

(require racket/set
         "private/common-requirements.rkt"
         "load.rkt"
         (only-in "convenience.rkt" lo-or-not))


(provide
 (struct-out portion)
 (struct-out allocation)
 allocation-equity-line)

(define (pos-real? x)
  (and (real? x) (positive? x)))

(struct portion (ticker amount)
  #:transparent
  #:guard
  (λ (ticker amount type-name)
    (cond
      [(not (string? ticker))
       (raise-argument-error type-name "string?" ticker)]
      [(not (pos-real? amount))
       (raise-argument-error type-name "positive?" amount)]
      [else
       (values ticker amount)])))


(struct allocation (date portions)
  #:transparent
  #:guard
  (λ (date portions type-name)
    (cond
      [(not (jdate? date))
       (raise-argument-error type-name "jdate?" date)]
      [(not (and (pair? portions) (andmap portion? portions)))
       (raise-argument-error type-name "(listof? portion?)" portions)]
      [ else
        (values date portions)])))


(define (normalize-allocation a)
  (define portions (allocation-portions a))
  (define tickers (map portion-ticker portions))
  (define amounts (map portion-amount portions))
  (define sum (for/sum ((n (in-list amounts))) n))
  (allocation (allocation-date a)
              (for/list ((ticker (in-list tickers))
                         (amount (in-list amounts)))
                (portion ticker (/ amount sum)))))



(struct holding (ticker shares)
  #:transparent
  #:guard
  (λ (ticker shares type-name)
    (cond
      [(not (string? ticker))
       (raise-user-error type-name "string?" ticker)]
      [(not (pos-real? shares))
       (raise-user-error type-name "positive?" shares)]
      [else
       (values ticker shares)])))


(struct portfolio (date holdings)
  #:transparent
  #:guard
  (λ (date holdings type-name)
    (cond
      [(not (jdate? date))
       (raise-argument-error type-name "jdate?" date)]
      [(not (list? holdings))
       (raise-argument-error type-name "(listof? holding?)" holdings)]
      [(not (andmap holding? holdings))
       (raise-argument-error type-name "(listof? holding?)" holdings)]
      [else
       (values date holdings)])))



(define (allocation-equity-line allocations dates)
  (define ds (dateset->set dates))
  (define portfolios-by-date
    (for/hash ((p (in-list (allocation-history->portfolio-history allocations))))
      (define dt (portfolio-date p))
      (unless (set-member? ds dt)
        (raise-user-error 'allocation-equity-line
                          "portfolio point is not part of dateset at ~a"
                          (jdate->text dt)))
      (values dt p)))
  (define dv (dateset-vector dates))
  (define cash 1.0)
  (define current-portfolio '())
  (obs->series
   (for/list ((dt (in-vector dv)))
     (define value
       (+ cash
          (for/sum ((h (in-list (portfolio-holdings current-portfolio))))
            (define shares (holding-shares h))
            (define ticker (holding-ticker h))
            (define price ((series-function (lo ticker)) dt))
            (* shares price))))
     (define new-portfolio (hash-ref portfolios-by-date dt #f))
     (when new-portfolio
       (set! current-portfolio new-portfolio)
       (set! cash
             (if (pair? current-portfolio)
                 0
                 value)))
     (observation dt value))))

(define (allocation-history->portfolio-history _allocations)

  (define allocations
    (map normalize-allocation
         (sort _allocations < #:key allocation-date)))

  (define date-ticker-price
    (let ((h (make-hash)))
      
      (λ (date ticker)
        (define (price-loading-thunk)
          (series-function
           (or (lo-or-not ticker)
               (raise-user-error 'allocation-history->portfolio-history
                                 "~a price history cannot be loaded"
                                 ticker))))
        (define price-function
          (hash-ref! h ticker price-loading-thunk))
        
        (or (price-function date)
            (raise-user-error 'allocation-history->portfolio-history
                              "~a has no price data at ~a"
                              ticker
                              (jdate->text date))))))

  
  (for/fold ((acc '())
             (cash 1.0)
             #:result (reverse acc))
            ((a (in-list allocations)))
    (define date (allocation-date a))
    (define portions (allocation-portions a))
    (define holdings (if (pair? acc) (portfolio-holdings (car acc)) '()))
    (define portfolio-value
      (+ cash
         (for/sum ((h (in-list holdings)))
           (define ticker (holding-ticker h))
           (define shares (holding-shares h))
           (define price (date-ticker-price date ticker))
           (* shares price))))
    (define new-holdings
      (map (λ (portion)
             (define ticker (portion-ticker portion))
             (define price (date-ticker-price date ticker))
             (define amount (portion-amount portion))
             (define dollars-for-buy (* amount portfolio-value))
             (define shares-to-buy (/ dollars-for-buy price))
             (holding ticker shares-to-buy))
           portions))
    (define new-cash (if (pair? new-holdings) 0 portfolio-value))
    (define new-portfolio (portfolio date new-holdings))
    (values (cons new-portfolio acc) new-cash)))


(module+ main
  
  (define (dump-allocations allocations)
    (for ((a (in-list allocations)))
      (printf "~a" (jdate->text (allocation-date a)))
      (for ((p (in-list (allocation-portions a))))
        (printf " ~a:~a" (portion-ticker p) (portion-amount p)))
      (printf "\n")))
  
  (define (dump-portfolios portfolios)
    (for ((p (in-list portfolios)))
      (printf "~a" (jdate->text (portfolio-date p)))
      (for ((h (in-list (portfolio-holdings p))))
        (printf " ~a:~a" (holding-ticker h) (holding-shares h)))
      (printf "\n")))
  
  (define allocations
    (list
     (allocation (->jdate '2017-12-26)
                 (list (portion "IBM" 30)
                       (portion "SPY" 75)))
     (allocation (->jdate '2017-1-19)
                 (list (portion "IBM" 123)
                       (portion "SPY" 339)))
     (allocation (->jdate '2018-2-8)
                 (list (portion "IBM" 22)
                       (portion "SPY" 88)))))

  (printf "raw\n")
  (dump-allocations allocations)
  (printf "normalized\n")
  (dump-allocations (map normalize-allocation allocations))
  (printf "\n\n")
  (dump-portfolios (allocation-history->portfolio-history allocations)))



