#lang racket/base

(provide render)

(require racket/class
         racket/draw
         (only-in racket/format ~a ~r)
         (only-in racket/list first second)
         (only-in racket/function identity)
         "private/chart-structs.rkt"
         "signals.rkt"
         "min-max.rkt"
         "private/with-dc-state.rkt"
         "private/common-requirements.rkt")


(define (get-label-x label width dates)
  (define x-spec (hash-ref (component-attrs label) 'x))
  (define date (->jdate x-spec #:check? #t))
  (cond
    (date (let* ((fd (first-date dates))
                 (ld (last-date dates))
                 (denominator (+ 1 (- ld fd)))
                 (numerator (- date fd)))
            (* width (/ numerator denominator))))
    ((∝? x-spec) (* width (∝-value x-spec) 1.0))
    ((number? x-spec) x-spec)
    (else (raise-user-error "cannot figure what x-spec ~a means" x-spec))))

(define (get-label-y label height)
  (define y-spec (hash-ref (component-attrs label) 'y))
  (- height
     (cond
       ((∝? y-spec)
        (* (∝-value y-spec) 1.0 height))
       (else
        (raise-user-error "only relative-y supported for now")))))

(define (compass-drawtext dc text x y compass)
  
  (define-values (W H baseline _)
    (send dc get-text-extent text))
  
  (define y-shift
    (case compass
      [ (NW N NE) (- H baseline) ]
      [ (W CENTER E) (/ (- H baseline) 2)]
      [ (SW S SE) 0 ]
      [ else (raise-user-error "bad compass") ]))
  
  (define x-shift
    (case compass
      [ (N CENTER S) (/ W 2) ]
      [ (NW W SW) W ]
      [ (SE E NE) 0 ]
      [ else (raise-user-error "bad compass") ]))

  (send dc draw-text text (- x x-shift) (- y y-shift)))

(define (make-snagger item)
  (λ (key)
    (hash-ref (component-attrs item) key #f)))



(define (draw-labels #:dc dc
                     #:labels labels
                     #:width width
                     #:height height
                     #:dates dates)
  (for ((label (in-list labels)))
    
    (define snag (make-snagger label))
    (define compass (snag 'compass))
    (define color (snag 'color))
    (define x (get-label-x label width dates))
    (define y (get-label-y label height))
    (define font-spec (snag 'font))

    (with-dc-state dc
      
      (when color
        (send dc set-text-foreground (~a color)))
      
      (when font-spec
        (let* ((prior-font  (send dc get-font))
               (prior-size  (send prior-font get-size))
               (unmagged    (hash-ref font-spec 'size prior-size))
               (size        (* unmagged (hash-ref font-spec 'mag 1.0)))
               (face        (hash-ref font-spec 'face (send prior-font get-face)))
               (style       (send prior-font get-style))
               (weight      (send prior-font get-weight)))

          (send dc
                set-font
                (make-font #:size size
                           #:face face
                           #:style style
                           #:weight weight))))
      
      (compass-drawtext dc (label-component-text label) x y compass))))

(define (draw-signals #:dc dc
                      #:sigplot sigplot
                      #:coordinate-system coordinate-system
                      #:dates dates)
  
  (define signals (to-signals (sigplot-component-signals sigplot) #:dates dates))
  (define series (series-function (sigplot-component-series sigplot)))
  (define snag (make-snagger sigplot))
  (define length (snag 'length))
  (define sell-pen (send the-pen-list find-or-create-pen "red" 0 'solid))
  (define buy-pen (send the-pen-list find-or-create-pen "green" 0 'solid))

  (for ((ob (in-list (series->obs dates signals))))
    (define dt (ob-d ob))
    (define sig-val (ob-v ob))
    (define x (send coordinate-system get-x dt))
    (define y (send coordinate-system get-y (series dt)))
    (if (> sig-val 0)
        (send* dc
          (set-pen buy-pen)
          (draw-line x y x (+ y length)))
        (send* dc
          (set-pen sell-pen)
          (draw-line x y x (- y length))))))


(define (draw-plot #:dc dc
                   #:plot plot
                   #:coordinate-system coordinate-system
                   #:dates dates)
  
  (define obs (series->obs dates (plot-component-series plot)))
  (define snag (make-snagger plot))
  (define style (snag 'style))
  (define color (snag 'color))
  (define thickness (snag 'thickness))
  (define pen (send the-pen-list find-or-create-pen (~a color) thickness style))

  (define xys
    (for/list ((ob (in-list obs)))
      (define x (send coordinate-system get-x (ob-d ob)))
      (define y (send coordinate-system get-y (ob-v ob)))
      (make-object point% x y)))

  (send dc set-pen pen)
  (send dc draw-lines xys))


(define (get-desirable-unit min max desired-count)
  (define distance (- max min))
  (define possible-units
    (for*/list ((mag (in-range -8 9))
                (fac (in-list '(1 2 5/2 5))))
      (* fac (expt 10 mag))))
  (define unit-diffs
    (for/list ((unit (in-list possible-units)))
      (list unit (abs (- desired-count (/ distance unit))))))
  (define sorted
    (sort unit-diffs (λ (a b)
                       (< (second a)
                          (second b)))))
  (first (first sorted)))



(define coordinate-system%
  (class object%
    
    (init width
          height
          padding
          dates
          min-val
          max-val)

    (define _min-date (first-date dates))
    (define _max-date (last-date dates))
    
    (define scale-value
      (if (and (> min-val 1)
               (> (/ max-val min-val) 2))
          (λ (n)
            (log (+ n 1)))
          identity))

    (define _min-val min-val)
    (define _max-val max-val)
    (define _scaled-max-val (scale-value max-val))
    (define _scaled-min-val (scale-value min-val))
    (define _width (* 1.0 width))
    (define _height (* 1.0 height))
    (define _top (padding-top padding))
    (define _left (padding-left padding))
    (define _padded-width (- width _left (padding-right padding)))
    (define _padded-height (- height _top (padding-bottom padding)))

    (super-new)

    (define/private (get-ratio-x r)
      (* r _width))

    (define/private (get-date-x d)
      (+ _left
         (* _padded-width
            (/ (- d _min-date)
               (- _max-date _min-date)))))

    (define/public (get-x item)
      (* 1.0
         (cond
           ((∝? item)
            (get-ratio-x (∝-value item)))
           (else
            (get-date-x item)))))

    (define/private (get-ratio-y r)
      (- 1 r _height))

    (define/private (get-data-y d)
      (+ _top
         (* _padded-height
            (- 1 (/ (- (scale-value d) _scaled-min-val)
                    (- _scaled-max-val _scaled-min-val))))))

    (define/public (get-y item)
      (* 1.0
         (cond
           ((∝? item)
            (get-ratio-y (∝-value item)))
           (else
            (get-data-y item)))))


    (define/public (get-y-axis-values #:desired-count (desired-count 10))
      (define unit (get-desirable-unit _min-val _max-val desired-count))
      (sort
       (append (for/list ((n (in-range 0 _max-val unit))
                          #:when (< _min-val n _max-val)) n)
               (for/list ((n (in-range 0 _min-val (- unit)))
                          #:when (< _min-val n _max-val)) n))
       <))))


(define (commonly-formatted nums)
  (define strings (map ~r nums))
  (define stripped (map (λ (s) (regexp-replace #px"^[0-9]+\\.?" s "")) strings))
  (define max-length (apply max (map string-length stripped)))
  (if (zero? max-length)
      strings
      (map (λ (n) (~r n #:precision (list '= max-length))) nums)))

(define (get-years dates)
  (filter (λ (dt)
            (<= (first-date dates) dt (last-date dates)))
          (for/list ((y (in-range 1900 2030)))
            (ymd->jdate y 1 1))))

(define (get-quarters dates)
  (filter (λ (dt)
            (<= (first-date dates) dt (last-date dates)))
          (for*/list ((y (in-range 1900 2030))
                      (m (in-list '(1 4 7 10))))
            (ymd->jdate y m 1))))

(define (get-months dates)
  (filter (λ (dt)
            (<= (first-date dates) dt (last-date dates)))
          (for*/list ((y (in-range 1900 2030))
                      (m (in-range 1 13)))
            (ymd->jdate y m 1))))


(define (draw-dates #:dc dc #:dates dates #:coordinate-system coordinate-system)

  (define years (get-years dates))
  (define quarters (get-quarters dates))
  (define months (get-months dates))

  (for ((dt (in-list years)))
    (define two-digit (substring (~a (ymd-year (jdate->ymd dt))) 2))
    (define-values (w h b _) (send dc get-text-extent two-digit))
    (define x (send coordinate-system get-x dt))
    (send* dc
      (draw-line x 0 x 5)
      (draw-text two-digit (- x (/ w 2)) 6)))

  (for ((dt (in-list quarters)))
    (define two-digit (substring (~a (jdate-year dt)) 2))
    (define-values (w h b _) (send dc get-text-extent two-digit))
    (define x (send coordinate-system get-x dt))
    (send* dc
      (draw-line x 0 x 3.5))))



(define (draw-clip #:dc dc
                   #:clip clip
                   #:height height
                   #:width width
                   #:dates dates)
  
  (define snag (make-snagger clip))
  (define background (snag 'background))
  (define show-dates (snag 'show-dates))
  (define padding (snag 'padding))
  (define brush
    (if background
        (send the-brush-list find-or-create-brush (~a background) 'solid)
        (send the-brush-list find-or-create-brush "red" 'transparent)))
  (define labels (filter label-component? (container-children clip)))
  (define plots (filter plot-component? (container-children clip)))
  (define sigplots (filter sigplot-component? (container-children clip)))
  (define min-datum
    (apply min
           (map (λ (p)
                  (ob-v (min-ob (plot-component-series p) #:dates dates))) plots)))
  (define max-datum
    (apply max
           (map (λ (p)
                  (ob-v (max-ob (plot-component-series p) #:dates dates))) plots)))

  (define plotting-height (- height (if show-dates 25 0)))

  (define coordinate-system
    (new coordinate-system%
         [ width width ]
         [ height plotting-height ]
         [ padding padding ]
         [ dates dates ]
         [ min-val min-datum ]
         [ max-val max-datum ]))

  (send* dc
    (set-brush brush)
    (set-pen (send the-pen-list find-or-create-pen "black" 0 'solid))
    (draw-rectangle 0 0 width plotting-height))

  (for ((p (in-list plots)))
    (with-dc-state dc
      (draw-plot #:dc dc
                 #:plot p
                 #:coordinate-system coordinate-system
                 #:dates dates)))

  (for ((sp (in-list sigplots)))
    (with-dc-state dc
      (draw-signals #:dc dc
                    #:sigplot sp
                    #:coordinate-system coordinate-system
                    #:dates dates)))

  (let ((axis-values (send coordinate-system get-y-axis-values)))
    (for ((n (in-list axis-values))
          (fn (in-list (commonly-formatted axis-values))))
      (define physical-y (send coordinate-system get-y (* 1.0 n)))
      (define-values (w h b _) (send dc get-text-extent fn))
      (send* dc
        (draw-line 0 physical-y -3 physical-y)
        (draw-text fn  (- 0 6 w) (- physical-y (/ h 2)))
        (draw-line width physical-y (+ width 3) physical-y)
        (draw-text fn (+ 6 width) (- physical-y (/ h 2))))))

  (with-dc-state dc
    (let ((fn (send dc get-font)))
      (send* dc
        (set-font (make-font #:size (* (send fn get-size) 3/4)
                             #:face (send fn get-face)))
        (translate 0 plotting-height)))
    (draw-dates #:dc dc
                #:dates dates
                #:coordinate-system coordinate-system))

  (with-dc-state dc
    (draw-labels #:dc dc
                 #:labels labels
                 #:width width
                 #:height plotting-height
                 #:dates dates)))



(define (draw-clips #:dc dc
                    #:clips clips
                    #:width-including-gutters width-including-gutters
                    #:height height
                    #:dates dates)

  (define clip-weights (for/list ((c (in-list clips)))
                         (hash-ref (component-attrs c) 'weighting)))
  
  (define total-weight (for/sum ((cw clip-weights)) cw))
  

  (define left-axis-gutter (for/or ((c (in-list clips)))
                             (if (hash-ref (component-attrs c) 'left-axis #f)
                                 50
                                 0)))

  (define right-axis-gutter (for/or ((c (in-list clips)))
                              (if (hash-ref (component-attrs c) 'right-axis #f)
                                  50
                                  0)))

  (define width (- width-including-gutters
                   left-axis-gutter
                   right-axis-gutter))

  (send dc translate left-axis-gutter 0)

  (for ((clip (in-list clips)))

    (define snag (make-snagger clip))
    (define clip-height (* height (/ (snag 'weighting) total-weight)))

    (draw-clip #:dc dc
               #:clip clip
               #:height clip-height
               #:width width
               #:dates dates)

    (send dc translate 0 clip-height)))


(define sky-brush
  (new brush%
       [ gradient  (new linear-gradient%
                        [ x0 0 ]
                        [ y0 0 ]
                        [ x1 800 ]
                        [ y1 600 ]
                        [ stops (list (list 0 (make-object color% 255 255 255))
                                      (list 1 (make-object color% #xC3 #xE3 #xFF)))])]))

(define (get-brush id)
  (cond
    ((not id) #f)
    ((eq? 'sky id) sky-brush)
    (else (send the-brush-list find-or-create-brush (~a id) 'solid))))


(define (render chart #:dc (original-dc #f))

  (define snag (make-snagger chart))

  (define doc-page   (snag            'doc-page))
  (define width      (snag            'width))
  (define height     (snag            'height))
  (define background (get-brush (snag 'background)))
  (define border     (snag            'border))
  (define dates      (snag            'dates))

  (define children (container-children chart))
  (define labels   (filter label-component? children))
  (define clips    (filter clip-container? children))

  (define bm (and (not original-dc) (make-bitmap width height)))
  (define dc (or original-dc (new bitmap-dc% [ bitmap bm ])))

  (when doc-page
    (send* dc
      (start-doc "untitled")
      (start-page)))

  (send dc set-font (make-font #:size (/ width 70) #:face "Optima"))

  (unless original-dc
    (send dc set-smoothing 'aligned))

  ; chart background/border
  (with-dc-state dc

    (when background
      (send* dc
        (set-pen (send the-pen-list find-or-create-pen "red" 0 'transparent))
        (set-brush background)
        (draw-rectangle 0 0 width height)))

    (when border
      (send* dc
        (set-pen (send the-pen-list find-or-create-pen "black" 0 'solid))
        (set-brush (send the-brush-list find-or-create-brush "black" 'transparent))
        (draw-rectangle 0 0 width height))))

  ; draw individual plot sections
  (with-dc-state dc
    (let* ((p (snag 'padding))
           (h (- height (padding-top p) (padding-bottom p)))
           (w (- width  (padding-left p) (padding-right p))))
      (send dc translate (padding-left p) (padding-top p))
      (draw-clips #:dc dc
                  #:clips clips
                  #:width-including-gutters w
                  #:height h
                  #:dates dates)))

  ; draw labels
  (with-dc-state dc
    (draw-labels #:dc dc #:labels labels #:width width #:height height #:dates dates))

  (when doc-page
    (send* dc
      (end-page)
      (end-doc)))
  
  (or bm
      (void)))



