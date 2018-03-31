
#lang racket/base

(require racket/set
         "private/common-requirements.rkt"
         "load.rkt"
         (only-in "convenience.rkt" lo-or-not)
         "constant.rkt")


(provide
 (struct-out portion)
 (struct-out allocation)
 allocation-equity-line)

(define (pos-real? x)
  (and (real? x) (positive? x)))

(struct portion (series amount)
  #:transparent
  #:guard
  (λ (series amount type-name)
    (cond
      [(not (series? series))
       (raise-argument-error type-name "series?" series)]
      [(not (pos-real? amount))
       (raise-argument-error type-name "positive?" amount)]
      [else
       (values series amount)])))


(struct allocation (date portions)
  #:transparent
  #:guard
  (λ (date portions type-name)
    (cond
      [(not (jdate? date))
       (raise-argument-error type-name "jdate?" date)]
      [(not (and (pair? portions) (andmap portion? portions)))
       (raise-argument-error type-name "(listof? portion?)" portions)]
      [else
       (values date portions)])))

(define CASH (constant 1))

(define (normalize-allocation a)
  (define portions (allocation-portions a))
  (define seriesz (map portion-series portions))
  (define amounts (map portion-amount portions))
  (define sum (for/sum ((n (in-list amounts))) n))
  (if (null? portions)
      (allocation (allocation-date a)
                  (list (portion CASH 1)))
      (allocation (allocation-date a)
                  (for/list ((series (in-list seriesz))
                             (amount (in-list amounts)))
                    (portion series (/ amount sum))))))



(struct holding (series shares)
  #:transparent
  #:guard
  (λ (series shares type-name)
    (cond
      [(not (series? series))
       (raise-user-error type-name "series?" series)]
      [(not (pos-real? shares))
       (raise-user-error type-name "positive?" shares)]
      [else
       (values series shares)])))


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


(define CASH-HOLDING (holding CASH 1))

(define (allocation-equity-line allocations)
  (define portfolios-by-date
    (for/hash ((p (in-list (allocation-history->portfolio-history allocations))))
      (values (portfolio-date p) p)))
  (define fd (apply min (hash-keys portfolios-by-date)))
  (define ld (today 1))
  (define current-portfolio (portfolio (- fd 1) (list CASH-HOLDING)))
  (obs->series
   (for/list ((dt (in-range fd ld)))
     (define holding-valuations
       (let ((tmp (for/list ((h (in-list (portfolio-holdings current-portfolio))))
                    (define shares (holding-shares h))
                    (define series (holding-series h))
                    (define price ((series-function series) dt))
                    (and price (* shares price)))))
         (and (andmap real? tmp) tmp)))
     (define new-portfolio (hash-ref portfolios-by-date dt #f))
     (define value
       (and holding-valuations
            (apply + holding-valuations)))
     (when (and (not value) new-portfolio)
       (raise-user-error 'allocation-equity-line
                         "missing series observation at allocation date ~a"
                         (jdate->text dt)))
     (when new-portfolio
       (set! current-portfolio new-portfolio))

     (and value
          (observation dt value)))))

(define (allocation-history->portfolio-history _allocations)

  (define allocations
    (map normalize-allocation
         (sort _allocations < #:key allocation-date)))

  (define holdings (list (holding CASH 1)))

  (for/list ((a (in-list allocations)))
    (define date (allocation-date a))
    (define portions (allocation-portions a))
    (define portfolio-value
      (for/sum ((h (in-list holdings)))
        (define sf (series-function (holding-series h)))
        (define shares (holding-shares h))
        (define price (sf date))
        (* shares price)))
    (set! holdings
          (map (λ (portion)
                 (define series (portion-series portion))
                 (define sf (series-function series))
                 (define price (sf date))
                 (define amount (portion-amount portion))
                 (define dollars-for-buy (* amount portfolio-value))
                 (define shares-to-buy (/ dollars-for-buy price))
                 (holding series shares-to-buy))
               portions))
    (portfolio date holdings)))


(define ALLOCATIONS (list
                     (allocation (->jdate '2017-12-26)
                                 (list (portion (lo "IBM") 30)
                                       (portion (lo "FXR") 33)
                                       (portion (lo "SPY") 75)))
                     (allocation (->jdate '2017-1-19)
                                 (list (portion (lo "IBM") 123)
                                       (portion (lo "FXO") 333)
                                       (portion (lo "SPY") 339)))
                     (allocation (->jdate '2018-2-8)
                                 (list (portion (lo "IBM") 22)
                                       (portion (lo "SPY") 88)))))

(module+ main

    (require "load.rkt"
             "dump.rkt")
  
    (define (dump-allocations allocations)
      (for ((a (in-list allocations)))
        (printf "~a" (jdate->text (allocation-date a)))
        (for ((p (in-list (allocation-portions a))))
          (printf " ~a:~a" (portion-series p) (portion-amount p)))
        (printf "\n")))
  
    (define (dump-portfolios portfolios)
      (for ((p (in-list portfolios)))
        (printf "~a" (jdate->text (portfolio-date p)))
        (for ((h (in-list (portfolio-holdings p))))
          (printf " ~a:~a" (holding-series h) (holding-shares h)))
        (printf "\n")))
  
    (define allocations
      (list
       (allocation (->jdate '2017-12-26)
                   (list (portion (lo "IBM") 30)
                         (portion (lo "FXR") 33)
                         (portion (lo "SPY") 75)))
       (allocation (->jdate '2017-1-19)
                   (list (portion (lo "IBM") 123)
                         (portion (lo "FXO") 333)
                         (portion (lo "SPY") 339)))
       (allocation (->jdate '2018-2-8)
                   (list (portion (lo "IBM") 22)
                         (portion (lo "SPY") 88)))))

    (printf "raw\n")
    (dump-allocations allocations)
    (printf "normalized\n")
    (dump-allocations (map normalize-allocation allocations))
    (printf "\n\n")
    (dump-portfolios (allocation-history->portfolio-history allocations))
    (define result (allocation-equity-line allocations))
    (define SPY (lo "SPY"))
    (with-dates result
        (dump result)))



