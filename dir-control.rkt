#lang racket/base
(require racket/gui/base
         racket/class)

(define gap 15) ; the number of pixels to increase x to seperate segments
(define left-margin 8); margin between left of segment and text start
(define highlighted-colour "orange")

(define dir-control%
  (class canvas%
    (inherit refresh get-dc)
    (init [callback (λ (ce e) (void))])
    (define path-elements '())
    (define mouse-pos 10000)

    (define/public-final (set-path _path)
      (set! path-elements (explode-path (simplify-path _path)))
      (refresh))

    (define (highlight-if-hover mouse-xpos) (set! mouse-pos mouse-xpos))
    (define (select-action mouse-xpos) 
      (displayln mouse-xpos))


    
    (define/override (on-paint)
      (define dc (get-dc))
      (define old-brush (send dc get-brush))
      (define old-pen (send dc get-pen))

      (send dc set-brush "silver" 'solid)
      (send dc set-pen "black" 1 'solid)
      
      (for/fold ([x 0])
                ([pe (in-list path-elements)])
        (define (draw-background-segment
                 a-dc side-width text-height xoffset yoffset colour)
          (send dc set-brush colour 'solid)
          (define height (+ text-height (/ text-height 5)))
          (send a-dc draw-polygon
                (list
                 (cons 0 0)
                 (cons side-width 0)
                 (cons (+ side-width 5) (/ height 2))
                 (cons side-width height)
                 (cons 0 height)
                 (cons 5 (/ height 2)))
                xoffset	 	 	 	 
                yoffset
                ))
        (define s
          (cond
            [(relative-path? pe)
             (path-element->string pe)]
            [else
             ;; should be the first one; maybe do this differently?
             (path->string pe)]))
        (define-values (pw ph pd pa) (send dc get-text-extent s))
        (draw-background-segment dc
                                 (+ pw 10) ; 
                                 ph ; text height
                                 x ; x offset
                                 0 ; y offset
                                 (if (and (>= mouse-pos x)(<= mouse-pos (+ x pw 10))) "orange" "silver")

                                 )
        (cond
          [(= x 0)
           (send dc draw-text s (+ x left-margin) 0)
           (+ x gap pw)]
          [else
           (send dc draw-text s (+ x left-margin) 0)
           (+ x gap pw)]))
      (send dc set-brush old-brush)
      (send dc set-pen old-pen))


    (define/override (on-event me)
      (define mouse-xpos (send me get-x))
      (case (send me get-event-type)
        [(motion) (highlight-if-hover mouse-xpos)]
        [(left-down) (select-action mouse-xpos)])
      (on-paint))
    (super-new [style '()])
    (send (get-dc) set-font small-control-font)))


(module+ main
  (define f (new frame% [width 400] [height 100] [label ""]))
  (define c (new dir-control% [parent f]))
  (send c set-path (collection-file-path "base.rkt" "racket"))
  (send c set-path (current-directory-for-user))
  (send f show #t))