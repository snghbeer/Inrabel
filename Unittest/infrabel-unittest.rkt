#lang racket
(require rackunit
         "../INFRABEL/INFRABEL.rkt"
         "../INFRABEL/infrabel-components/interface.rkt"
         rackunit/text-ui
         ;rackunit/gui
         )
(define infrabel (infrabel-adt))

(define 1-1 13)
(define 1-2 14)
(define 1-3 15)
(define 1-4 17)
(define 1-5 18)
(define 1-6 19)
(define 1-7 21)
(define 1-8 22)
(define 2-1 29)
(define 2-2 30)
(define 2-3 31)
(define 2-4 32)
(define 2-5 33)
(define 2-6 34)
(define 2-7 35)
(define 2-8 36)

;Initialize hardware mode
((infrabel 'mode) "Setup-hardware")
(sleep 1)
(start)
(sleep 1)

(define (has-adt? ids)
  (let ((res #t)
        (switch? (infrabel 'switch?)))
    (for-each (lambda (id)
                (if (switch? id)
                    (if (eq? 'S-2-3 id)
                        (let* ((s-2 ((infrabel 'get) 2)) ;om S-2-3 manueel te checken
                               (s-3 ((infrabel 'get) 3))
                               (s-2-id (s-2 'id))
                               (s-3-id (s-3 'id)))
                          (when (not (and (eq? s-2-id 'S-2)
                                          (eq? s-3-id 'S-3)))
                            (set! res #f)))
                        (let* ((str-id (symbol->string id))
                               (str-idx (substring str-id 2))
                               (idx (string->number str-idx))
                               (switch ((infrabel 'get) idx))
                               (switch-id (switch 'id)))
                          (when (not (eq? id switch-id))
                            (set! res #f))))
                    (let* ((block ((infrabel 'getb) id))
                           (block-id (block 'id)))
                      (when (not (eq? id block-id))
                        (set! res #f)))
                    ))
              ids)
    res))

(define tests
  (test-suite
   "Test for data update"
   (test-case
    "Checks if each detection-block has an ADT"
    (let ((blocks (get-detection-block-ids)))
      (check-true (has-adt? blocks))))
   (test-case
    "Checks if each switch has an adt-object"
    (let ((switches (get-switch-ids)))
      (check-true (has-adt? switches))))
   (test-case
    "Add loco"
    ((infrabel 'add-loco!) (vector "Add-loco!" "Test-loco" "1-5" "1-4" "2-7"))
    (check-true (not (null? (vector-ref (infrabel 'locos) 0)))))
   (test-case
    "Slow-down loco for dynamic speed"
    (let* ((loco (vector-ref (infrabel 'locos) 0))
           (oldspeed (loco 'speed)))
      (loco 'slow!)
      (let ((new-speed (loco 'speed)))
        (check-true (< new-speed oldspeed)))))
   (test-case
    "Delete loco"
    (check-true (not (null? (vector-ref (infrabel 'locos) 0))))
    ((infrabel 'delete-loco!) "0")
    (check-true (null? (vector-ref (infrabel 'locos) 0))))
   (test-case
    "Test for free path with unreserved switch(es)"
    (check-true ((infrabel 'free) '(21 19 6 5 7 2 3 8 4 35))) ;path from 1-7 to 2-7(hard scenario)
    (let ((random-switch ((infrabel 'get) 8)))
      (random-switch 'reserve!)
      (check-false ((infrabel 'free) '(21 19 6 5 7 2 3 8 4 35)))))
   (test-case
    "Checks if loco data updates with simulator"
    ((infrabel 'add-loco!) (vector "Add-loco!" "Test-loco" "1-5" "1-4" "2-4"))
    (let ((loco (vector-ref (infrabel 'locos) 0)))
      ((infrabel 'loco-speed!) (vector "Speed!" 0))
      (check-equal? (loco 'seg) (get-loco-detection-block 'Test-loco))
      ))
   (test-case
    "Checks if loco stops at destination"
    (let ((loco (vector-ref (infrabel 'locos) 0)))
      (when (and (eq? (loco 'seg) '2-4)
                 (eq? (loco 'seg) (loco 'dest)))
        (check-equal? (loco 'seg) (get-loco-detection-block 'Test-loco)) ;adt <-> simulator
        (check-true (loco 'paused)))))
   ))

(run-tests tests)
;(test/gui tests)