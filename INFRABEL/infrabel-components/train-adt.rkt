#lang racket

(provide make-train-adt)

(define (make-train-adt id prev-seg start-seg dest)
  (let ((curr-speed 0)
        (prev  (string->symbol prev-seg))
        (seg (string->symbol start-seg))
        (destination (string->symbol dest))
        (id (string->symbol id))
        (path '())
        (paused #t))


    (define (path! p)
      (set! path p))

    (define (pause!)
      (set! paused #t))

    (define (set-speed! speed)
      (set! curr-speed speed))

    (define (start-loco-again)
      (set! paused #f))

    (define (stop-loco)
      (pause!))

    (define (prev! new-prev)
      (set! prev new-prev))

    (define (pos! new-seg)
      (set! prev seg)
      (set! seg new-seg))

    (define (invert!)
      (set-speed! (- curr-speed)))

    (define (slow-down)
      (if (negative? curr-speed)
          (set! curr-speed (- (- (- curr-speed) 30)))
          (set! curr-speed (- curr-speed 30))))

    (define (train-dispatch m)
      (cond ((eq? m 'speed!) set-speed!)
            ((eq? m 'speed) curr-speed)
            ((eq? m 'start) (start-loco-again))
            ((eq? m 'invert!) (invert!))
            ((eq? m 'slow!) (slow-down))
            ((eq? m 'pos!) pos!)
            ((eq? m 'prev!) prev!)
            ((eq? m 'seg) seg)
            ((eq? m 'stop-loco) (stop-loco))
            ((eq? m 'id) id)
            ((eq? m 'path) path)
            ((eq? m 'path!) path!)
            ((eq? m 'pause!) (pause!))
            ((eq? m 'paused) paused)
            ((eq? m 'dest) destination)
            ((eq? m 'last-pos) prev)))
    train-dispatch))
    

  