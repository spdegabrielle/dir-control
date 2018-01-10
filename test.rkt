#lang racket/gui
(require "dir-control.rkt")
(define f (new frame% [width 400] [height 100] [label ""]))
(define dir-control
  (new dir-control%
       [parent f]
       [callback (λ (ce e)
                   (println )

                   (println (list-ref (send ce get-path-elements)
                                      (get-field path-index e)))
                   (flush-output))]))
(define pp (new horizontal-panel% [parent f]))
(send dir-control set-path (path-alist (current-directory-for-user)))
(send f show #t)
