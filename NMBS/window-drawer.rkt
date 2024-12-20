#lang racket
(require racket/gui/base)
(provide make-window-adt)

(define (make-window-adt)
        
  (define window (new frame%
                      [label "GUI"]
                      [width 500]
                      [height 600]
                      [style '(no-resize-border)]))
    
  (define (draw!)
    (send window show #t))

  ;;Buttons
  (define (make-button name func parent)
    (new button%	 
         [label name]	 
         [parent parent]
         [callback (lambda (button event)
                     (func))]
         [min-width 80]))

  ;;Panel
  (define (make-horiz-panel)
    (new horizontal-pane%	 
         [parent window]
         [alignment '(left top)]
         [min-height 100]
         ))

  (define (make-vert-panel parent)
    (new vertical-pane%	 
         [parent parent]
         [min-width 150]
         [min-height 100]
         [alignment '(left top)]
         ))

  (define (vert-gbp label parent)
    (new group-box-panel%	 
         [label label]	 
         [parent parent]
         [min-width 150]
         ;[style '(border)]
         ))	 

  (define (txt-field label parent fun)
    (new text-field%	 
         [label label]	 
         [parent parent]	 
         [callback fun]	 
         [init-value ""]	 
         [style '(single)]))

  (define (choices label parent fun choices)
    (new choice%	 
         [label label]	 
         [choices choices]	 
         [parent parent]	 
         [callback (lambda (callback event)
                     (fun))]
         [selection 0]
         ))

  (define (rb label parent fun choices)
    (new radio-box%	 
         [label label]	 
         [choices choices]	 
         [parent parent]
         [style '(vertical)]
         [callback (lambda (radio event)
                     (fun))]
         [selection #f]))

  (define (list-box fun label parent choices)
    (new list-box%	 
   	 	[label label]	 
   	 	[choices choices]	 
                [parent parent]	 
                [callback (lambda (callback event)
                            (fun))]	 
   	 	[style '(single variable-columns)]	 
   	 	[selection #f]
                ))

  (define (msg label parent)
    (new message%	 
         [label label]	 
         [parent parent]
         [auto-resize #t]))

  (define (window-dispatch m)
    (cond ((eq? m 'draw!) (draw!))
          ((eq? m 'h-panel) (make-horiz-panel))
          ((eq? m 'v-panel) make-vert-panel)
          ((eq? m 'gbp) vert-gbp)
          ((eq? m 'txt) txt-field)
          ((eq? m 'choices) choices)
          ((eq? m 'rb) rb)
          ((eq? m 'lb) list-box)
          ((eq? m 'window) window)
          ((eq? m 'make-button) make-button)
          ((eq? m 'msg) msg)))
  window-dispatch)


