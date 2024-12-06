#lang racket
(provide make-switch-adt)

(define (make-switch-adt id idx)
  (let* ((id id)
         (index idx)
         (state 1)
         (reserved #f))

    (define (switch!)
      (cond ((= state 1) (set! state 2))
            ((= state 2) (set! state 1))))

    (define (reserve!)
      (set! reserved #t))

    (define (unreserve!)
      (set! reserved #f))

    (define (sw-dispatch m)
      (cond ((eq? m 'id) id)
            ((eq? m 'idx) index)
            ((eq? m 'state) state)
            ((eq? m 'switch!) (switch!))
            ((eq? m 'reserved) reserved)
            ((eq? m 'reserve!) (reserve!))
            ((eq? m 'unreserve!) (unreserve!))))
    sw-dispatch))
    

  
