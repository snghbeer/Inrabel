#lang racket

(require "interface.rkt")
(provide make-block-adt)

(define (make-block-adt id idx)
  (let* ((id id)
         (idx idx)
         (loco '()))

    (define (add-loco loc)
      (set! loco loc))

    (define (update-loco new)
      (set! loco new))

    (define (bl-dispatch m)
      (cond ((eq? m 'id) id)
            ((eq? m 'aloco) add-loco)
            ((eq? m 'idx) idx)
            ((eq? m 'loco) loco)
            ((eq? m 'uloco) update-loco)))
    bl-dispatch))
    

  
