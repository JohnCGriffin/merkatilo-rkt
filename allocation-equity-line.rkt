
#lang racket/base

(require "private/common-requirements.rkt"
         "constant.rkt")


(provide
 (struct-out portion)
 (struct-out allocation)
 (contract-out
  [allocation-equity-line (->* ((listof allocation?))
                               (#:init 100)
                               series?)]))

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
       (raise-argument-error type-name "(listof portion?)" portions)]
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
       (raise-argument-error type-name "(listof holding?)" holdings)]
      [(not (andmap holding? holdings))
       (raise-argument-error type-name "(listof holding?)" holdings)]
      [else
       (values date holdings)])))



(define (allocation-equity-line allocations #:init (initial-value 100))
  
  (define portfolio-history
    (allocation-history->portfolio-history allocations initial-value))
  
  (define portfolios-by-date
    (for/hash ((p (in-list portfolio-history)))
      (values (portfolio-date p) p)))
  
  (define fd (apply min (hash-keys portfolios-by-date)))
  (define ld (today 1))
  (define current-portfolio (portfolio (- fd 1)
                                       (list (holding CASH initial-value))))
  (obs->series
   #:name "allocation-equity-line"
   (for/list ((dt (in-range fd ld)))
     (define holdings (portfolio-holdings current-portfolio))
     (define portfolio-valuation
       (let ((tmp (for/list ((h (in-list holdings)))
                    (define shares (holding-shares h))
                    (define series (holding-series h))
                    (define price ((series-function series) dt))
                    (and price (* shares price)))))
         (and (andmap real? tmp) tmp)))
     (define new-portfolio (hash-ref portfolios-by-date dt #f))
     (define value
       (and portfolio-valuation
            (apply + portfolio-valuation)))
     
     (when (and (not value) new-portfolio)
       (raise-user-error 'allocation-equity-line
                         "missing series observation at allocation date ~a"
                         (jdate->text dt)))
     
     (when new-portfolio
       (set! current-portfolio new-portfolio))

     (and value
          (observation dt value)))))


(define (allocation-history->portfolio-history _allocations initial-value)

  (define allocations
    (map normalize-allocation
         (sort _allocations < #:key allocation-date)))

  (define holdings (list (holding CASH initial-value)))

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





