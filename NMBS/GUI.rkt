#lang racket
(require "window-drawer.rkt")
(provide make-gui)

(define (make-gui)
  
  (define window (make-window-adt))

  (window 'draw!)
  (define panel1 (window 'h-panel))
  (define panel2 (window 'h-panel))
  (define panel3 (window 'h-panel))
  (define vpanel ((window 'gbp) "Start/Stop" panel1))
  (define vpanel2 ((window 'gbp) "Simulator" panel1))
  (define vpanel3 ((window 'gbp) "Add a loco" panel2))
  (define vpanel5 ((window 'gbp) "Loco" panel2))
  (define vpanel4 ((window 'gbp) "" panel3))
    
  (define pos ((window 'msg) "" vpanel5))
  
  (define (pos! msg)
    (send pos set-label msg)) 

  (define (button name fun panel)
    ((window 'make-button) name
                           fun
                           panel))

  (define (stop-button stop)
    (button "Stop" stop vpanel))
  
  (define (exit-button exit)
    (button "Exit" exit vpanel))

  (define loco-id
    ((window 'txt) "Loco-id"
                   vpanel3
                   void))
       
  (define modes (file->list "simulator.txt"))
  (define (mode fun) ((window 'rb) "Modes \n"
                                   vpanel2
                                   fun
                                   modes))

  (define start-seg ((window 'choices) "Start-segment "
                                  vpanel3
                                  void
                                  '()))

   (define prev-seg ((window 'choices) "Previous-segment "
                                      vpanel3
                                      void
                                      '()))

  (define dest ((window 'choices) "Destination "
                                  vpanel3
                                  void
                                  '()))

  (define (lb fun) ((window 'lb) fun
                           "Select loco " vpanel4 (list "Empty"
                                                       "Empty"
                                                       "Empty")))

  
  (define (gui-dispatch m)
    (cond ((eq? m 'btn) button)
          ((eq? m 'stop) stop-button)
          ((eq? m 'exit) exit-button)
          ((eq? m 'loco-id) loco-id)
          ((eq? m 'mode) mode)
          ((eq? m 'locos) lb)
          ((eq? m 'start-seg) start-seg)
          ((eq? m 'prev-seg) prev-seg)
          ((eq? m 'dest) dest)
          ((eq? m 'p1) panel1)
          ((eq? m 'p2) panel2)
          ((eq? m 'p3) panel3)
          ((eq? m 'vp1) vpanel)
          ((eq? m 'vp2) vpanel2)
          ((eq? m 'vp3) vpanel3)
          ((eq? m 'vp4) vpanel4)
          ((eq? m 'pos) pos)
          ((eq? m 'pos!) pos!)
          ))
  gui-dispatch)



  