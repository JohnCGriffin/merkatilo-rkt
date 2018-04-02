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


(define (holdings-valuation date holdings)
  (let ((tmp (for/list ((h (in-list holdings)))
               (define shares (holding-shares h))
               (define series (holding-series h))
               (define price ((series-function series) date))
               (and price (* shares price)))))
    (and (andmap real? tmp)
         (apply + tmp))))


(define (allocations->portfolios _allocations)

  (define allocations
    (map normalize-allocation
         (sort _allocations < #:key allocation-date)))

  (define holdings (list (holding CASH 1)))

  (for/list ((a (in-list allocations)))
    (define date (allocation-date a))
    (define portions (allocation-portions a))
    (define valuation (holdings-valuation date holdings))
    (set! holdings
          (map (λ (portion)
                 (define series (portion-series portion))
                 (define sf (series-function series))
                 (define price
                   (or (sf date)
                       (raise-user-error "missing price at ~a" (jdate->text date))))
                 (define amount (portion-amount portion))
                 (define dollars-for-buy (* amount valuation))
                 (define shares-to-buy (/ dollars-for-buy price))
                 (holding series shares-to-buy))
               portions))
    (portfolio date holdings)))


(define (allocation-equity-line allocations #:init (initial-value 100))
  
  (define portfolios (allocations->portfolios allocations))
  
  (define holdings-by-date
    (for/hash ((p (in-list portfolios)))
      (values (portfolio-date p)
              (portfolio-holdings p))))

  (define fd (portfolio-date (car portfolios)))
  (define ld (today 1))
  (define holdings (list (holding CASH 1)))
  
  (obs->series
   #:name "allocation-equity-line"

   (for/list ((dt (in-range fd ld)))
     
     (define valuation (holdings-valuation dt holdings))
     
     (define new-holdings (hash-ref holdings-by-date dt #f))
     
     (when new-holdings
       (unless valuation
         (raise-user-error 'allocation-equity-line
                           "missing series observation at allocation date ~a"
                           (jdate->text dt)))
       (set! holdings new-holdings))

     (and valuation
          (observation dt (* initial-value valuation))))))




;==================================================

(module* test racket/base
  (require rackunit
           (submod "..")
           "series-binop.rkt"
           "signals.rkt"
           "momentum.rkt"
           "constant.rkt"
           "calibrate.rkt"
           "obs-series.rkt"
           "dump.rkt"
           "equity-line.rkt"
           "series-logic.rkt"
           "private/test-support.rkt")


  ;; allocation-equity-line with all weight to either AAA-SERIES
  ;; or cash is the same as having signals on those dates.
  (check-not-exn
   (λ ()
     (with-dates AAA-SERIES
       (define CASH (constant 1))
       (define sigs (to-signals (mo AAA-SERIES 100)))
       (define sig-obs (series->obs (current-dates) sigs))
       (define allocations
         (map (λ (o)
                (define val (ob-v o))
                (define dt (ob-d o))
                (define S (if (positive? val) AAA-SERIES CASH))
                (allocation dt (list (portion S 1)))) sig-obs))
       (define a-equity (allocation-equity-line allocations))
       (define t-equity (equity-line AAA-SERIES sigs))
       (verify-equivalency a-equity t-equity))))

  (check-not-exn
   (λ ()
     (with-dates AAA-SERIES
       (define allocations
         (list
          (allocation (first-date)
                      (list (portion AAA-SERIES 1)
                            (portion BBB-SERIES 1)))))
       #;(dump (allocation-equity-line allocations))
       ; given the one-time allocation, the AAA-SERIES and BBB-SERIES gains
       ; should be averaged (50% each).  So twice the average should be
       ; the same as AAA-SERIES + BBB-SERIES.
       (verify-equivalency
        (mul 2 (allocation-equity-line allocations))
        (add (calibrate AAA-SERIES) (calibrate BBB-SERIES)))))))

