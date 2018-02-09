#lang racket/base

;; A chart is simply a data structure containing clips, labels and plots,
;; which are in turn simple data structures.  The structures are
;; eventually rendered (usually into SVG) via (render) which takes a
;; Racket graphics dc% instance.

;; The (chart) call is a keyword-centric wrapper over chart-container.
;; Likewise, the clip and label are wrappers over clip-container and
;; label-component respectively.


(provide font
         render
         label
         plot
         sigplot
         clip
         chart
         ∝
         %)

(require (only-in racket/list partition flatten)
         "private/chart-structs.rkt"
         "chart-render.rkt"
         "core/dates.rkt")

(define (% value)
  (∝ (/ value 100)))

; Automatically wrap plots in clips if directly inside chart
(define (wrap-plots children)
  (define-values (ps non-ps)
    (partition plot-component? children))
  (append (map clip ps) non-ps))


(define (chart #:dates      (dates      (current-dates))
               #:background (background 'white)
               #:font       (font        #f)
               #:border     (border      #t)
               #:width      (width       800)
               #:height     (height      600)
               #:doc-page   (doc-page    #t)
               #:padding    (padding     (make-padding #:top 60
                                                       #:right 8
                                                       #:bottom 8
                                                       #:left 8))
                                         
               . children)
  
  (chart-container `((background ,background)
                     (font       ,font      ,hash?)
                     (dates      ,dates     ,dateset?)
                     (border     ,border)
                     (doc-page   ,doc-page)
                     (width      ,width     ,number?)
                     (padding    ,padding   ,padding?)
                     (height     ,height    ,number?))
                   
                   (wrap-plots (flatten children))))



(define (clip #:weighting  (weighting   1)
              #:background (background 'white)
              #:show-dates (show-dates  #t)
              #:left-axis  (left-axis   #t)
              #:right-axis (right-axis  #t)
              #:time-label (time-label 'auto)
              #:padding    (padding    (make-padding #:top 16
                                                     #:right 8
                                                     #:bottom 16
                                                     #:left 8))
              . children)
  
  (clip-container `((background  ,background)
                    (weighting   ,weighting)
                    (left-axis   ,left-axis)
                    (right-axis  ,right-axis)
                    (time-label  ,time-label)
                    (padding     ,padding)
                    (show-dates  ,show-dates))
                  (flatten children)))


(define (label text
               #:font       (font       #f)
               #:color      (color      #f)
               #:compass    (compass   'NE)
               #:x          (x          #f)
               #:y          (y          #f))
  
  (let ((valid '(N NE E SE S SW W NW CENTER)))
    (unless (member compass valid)
      (raise-user-error 'label "~a is not of ~a" compass valid)))

  (label-component `((font       ,font)
                     (compass    ,compass)
                     (color      ,color)
                     (x          ,x)
                     (y          ,y))
                   text))

         
(define (font #:face   (face   #f)
              #:size   (size   #f)
              #:italic (italic #f)
              #:mag    (mag    #f))
  
  (when (and size mag)
    (raise-user-error "font size and mag are mutually exclusive"))

  (ifhash `((face   ,face   ,string?)
            (size   ,size   ,number?)
            (mag    ,mag    ,number?)
            (italic ,italic))))


(define (plot series
              #:color     (color 'black)
              #:thickness (thickness 0)
              #:style     (style 'solid))
  
  (plot-component `((color      ,color)
                    (thickness  ,thickness  ,number?)
                    (style      ,style      ,symbol?))
                  series))


(define (sigplot signals
                 #:series series
                 #:length (length 15))
  
  (sigplot-component `((length ,length))
                  series
                  signals))


(module+ main
  (require racket/class
           racket/draw
           "momentum.rkt"
           "series-binop.rkt"
           "signals.rkt"
           "load.rkt")

  (define cht
    (parameterize ((current-dates (dates (lo 'FXG))))
    (chart #:background 'sky
           #:border #t
           #:dates (dates (lo "FXG") #:first '2013-1-1)
           (label "x = ∝1/4, y = % 75" #:x (∝ 1/4) #:y (% 75))
           (label "Zonkity" #:x (∝ 2/3) #:y (∝ 9/10) #:color 'forestgreen)
           (label "BIG" #:x (∝ 2/3) #:y (% 90) #:color 'red #:compass 'SW)
           (label "9/11" #:x '2001-9-11 #:y (∝ 1/2))
           (label "Yesterday" #:font (font #:mag 1.5) #:x (∝ 4/5) #:y (∝ 1/3) #:compass 'NW)
           (clip (plot #:thickness 3 (lo "SPY"))
                 (label "CORNER" #:x (% 100) #:y (% 0) #:compass 'NW)
                 #:weighting 1.5)
           (clip
            (sigplot (to-signals (sub (mo (lo "IBM") 50) 1))
                     #:series (lo "IBM")
                     #:length 50 )
            (plot #:style 'dot #:color 'blue (lo "IBM"))))))


  (render cht #:dc (new svg-dc%
                        [ width 800 ]
                        [ height 600 ]
                        [ output "/tmp/sample.svg" ]
                        [ exists 'replace ])))
