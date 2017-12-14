#lang racket/base
(require racket/gui/base
         racket/class)
(provide dir-control%)
; path; listof paths
;(define (parent-paths path)
;(define-values (base name dir) (split-path path))
;  (cond
;    [(equal? base #f) path]
;    [])
;  )

(define dir-control-event%
  (class event%
    (init-field [path-index #f])
    (super-new)))

(define dir-control%
  (class canvas%
    (inherit refresh get-dc popup-menu set-canvas-background)
    (init [callback (λ (ce e)
                      (println (list-ref (send ce get-path-elements)
                                         (get-field path-index e)))
                      (flush-output))])
    (define path-elements '())
    (define path-index #f)
    (define current-hover #f)
    (define mouse-pos 0)
    (define gap 15) ; the number of pixels to increase x to seperate segments
    (define left-margin 8); margin between left of segment and text start
    (define highlighted "orange")
    (define/public-final (get-path-elements) path-elements)
    (define/public-final (set-path _path)
      (set! path-elements (explode-path (simplify-path _path)))
      (refresh))

    (define (highlight-if-hover mouse-xpos) (set! mouse-pos mouse-xpos))

    (define/override (on-paint)
      (define dc (get-dc))
      (define old-brush (send dc get-brush))
      (define old-pen (send dc get-pen))

      (send dc set-brush "silver" 'solid)
      (send dc set-pen "black" 1 'solid)
      
      (for/fold ([xoffset 0])
                ([(pe i) (in-indexed path-elements)])
        (define (draw-background-segment
                 a-dc side-width text-height xoffset yoffset colour)
          (define (segment-outline-list height side [indent (/ height 3)])
            `((               0 . 0)
              (           ,side . 0)
              (,(+ side indent) . ,(/ height 2))
              (           ,side . ,height)
              (               0 . ,height)
              (         ,indent . ,(/ height 2))))

          (send dc set-brush colour 'solid)
          (define height (+ text-height (/ text-height 5)))
          (send a-dc draw-polygon
                (segment-outline-list height side-width)
                xoffset yoffset))
        (define path-element-string
          (cond
            [(relative-path? pe)
             (path-element->string pe)]
            [else
             ;; should be the first one; maybe do this differently?
             (path->string pe)]))
        (define-values (width font-height pd pa)
          (send dc get-text-extent path-element-string))
        (draw-background-segment
         dc (+ width 10) font-height xoffset 0 ; y offset
         (cond [(<= xoffset mouse-pos (+ xoffset width 10)) (set! path-index i)
                                                            ; highlight on hover
                                                            highlighted]
               [else "Gainsboro"]))
        (cond
          [(= xoffset 0)
           (send dc draw-text path-element-string (+ xoffset left-margin) 0)
           (+ xoffset gap width)]
          [else
           (send dc draw-text path-element-string (+ xoffset left-margin) 0)
           (+ xoffset gap width)]))
      (send dc set-brush old-brush)
      (send dc set-pen old-pen))
    
    (super-new [style '()])
    
    (set-canvas-background (make-object color% "WhiteSmoke"))

    (define (select-action mouse-xpos)
      (callback this (new dir-control-event% [path-index path-index])))
    
    (define/override (on-event me)
      (define mouse-xpos (send me get-x))
      (case (send me get-event-type)
        [(motion) (highlight-if-hover mouse-xpos)
                  (refresh)]
        [(left-down) (select-action mouse-xpos)]))

    (send (get-dc) set-font small-control-font)))


(module+ main
  (define f (new frame% [width 400] [height 100] [label ""]))
  (define c (new dir-control% [parent f]))
  (define pp (new horizontal-panel% [parent f]))

  (send c set-path (collection-file-path "base.rkt" "racket"))
  (send c set-path (current-directory-for-user))
  (send f show #t))

#;(define (make-menu dir)
    (define (my-directory-list dir #:hidden [hidden #t])
      (if (not hidden)
          (filter (λ (p) (if (equal? (string-ref (path->string p) 0) #\.) #f #t))
                  (directory-list dir))
          (directory-list dir))))

(define p (new popup-menu% [title "hi title"]))

(define m1 
  (new menu-item%	 
       [label "jjj"]	 
       [parent p]	 
       [callback (λ (i j) void)]))
  
(define m2 
  (new menu-item%	 
       [label "sss"]	 
       [parent p]	 
       [callback (λ (i j) void)]))
  
(define m3 
  (new menu-item%	 
       [label "ddd"]	 
       [parent p]	 
       [callback (λ (i j) void)]))
