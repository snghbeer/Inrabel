#lang racket

(require compatibility/mlist
         "infrabel-components/interface.rkt"
         "infrabel-components/train-adt.rkt"
         "infrabel-components/switch-adt.rkt"
         "infrabel-components/block-adt.rkt"
         "a-d/config.rkt") ;avl tree

(provide infrabel-adt)

;;SERVER-adt FOR INFRABEL
;https://docs.racket-lang.org/reference/tcp.html?fbclid=IwAR1xtSGCi_Ef16sCfBwbtVcLXnH7jk4Wk_3QDLZsM8VZ7utzvS-AkzLoDK8
(define (server-adt) ;https://stackoverflow.com/questions/21598836/how-to-use-tcp-in-racket
  (let ((the-listener (tcp-listen 50))
        (otp '()))
    
    (define-values (in out) (tcp-accept the-listener))
  
    (define (loop)
      (thread (lambda ()
                (let ((output (read in)))
                  (when (not (eq? eof output))
                    (set! otp output)
                    (sleep 1/2)
                    (loop))))))
    
    (define (sendmsg msg) ;TCP
      (write msg out)
      (flush-output out))

    (define (clean-output!)
      (set! otp '()))

    (define (close-connection)
      (tcp-close the-listener))

    (loop)

    (define (server-dispatch m)
      (cond ((eq? m 'loop) (loop))
            ((eq? m 'otp) otp)
            ((eq? m 'otp-clean) (clean-output!))
            ((eq? m 'port) the-listener)
            ((eq? m 'send) sendmsg)
            ((eq? m 'close) (close-connection))))
    server-dispatch))

;;INFRABEL

(define (infrabel-adt)
  (let ((loco-idx 0) ;counter for locos
        (tree (avl)) ;avl tree to find elements in log n
        (locos (make-vector 3 '())); buffer for locos
        (blocks (make-vector 10 '())) ;buffer for blocks
        (switches (make-vector 10 '())) ;buffer for switches
        (sel 0) ;actual loco selection index in nmbs
        (mode '()) ;selected mode
        (counter-clockwise 0) ;loco-speed for going clockwise (to the left starting from above)
        (server (server-adt))) ;server for tcp communications
    
     
    ;;;HELP FUNCTIONS

    (define (get-block-switch-by-idx idx) ;find in o(log n)
      (let ((res (find tree idx)))
        (when (not (eq? #f res))
          (mcdr res))))

    (define (switch? id)
      (let* ((str (symbol->string id))
             (character (substring str 0 1)))
        (if (string=? "S" character)
            #t
            #f)))

    (define (get-switch id)
      (let* ((str (symbol->string id))
             (sidx (substring str 3))
             (idx (string->number sidx))
             (bucket (vector-ref switches idx))
             (res '()))
        (for-each (lambda (switch)
                    (when (equal? (switch 'id) id)
                      (set! res switch)))
                  bucket)
        res))

    (define (get-block id) ;get block adt by ID with last digit as bucket-index
      (cond ((= mode 0);find in bucket is in O(alpha) with alpha = bucket.length
             (let* ((str (symbol->string id))
                    (is-switch? (switch? id)))
               (if is-switch?
                   (get-switch id)
                   (let* ((sidx (substring str 2))
                          (idx (string->number sidx))
                          (bucket (vector-ref blocks idx))
                          (res '()))
                     (for-each (lambda (block)
                                 (when (equal? (block 'id) id)
                                   (set! res block)))
                               bucket)
                     res))))
            (else (let ((res '()))
                    (vector-map (lambda (block)
                                  (when (equal? (block 'id) id)
                                    (set! res block)
                                    res)) blocks)
                    res))
            ))

    (define (mymlist->list mlist) ;conversion of mutable list to list
      (let ((res '())) ;+ puts list in right order
        (mfor-each (lambda (el)
                     (set! res (cons el res)))
                   mlist)
        res))

    (define (vec-map proc v)
      (let loop ((vlen (vector-length v))
                 (idx 0))
        (when (< idx vlen)
          (proc (vector-ref v idx))
          (loop vlen (+ idx 1)))))

    ;;;LOCO

    (define (for-each-loco proc)
      (vec-map (lambda (loco) (when (not (null? loco))
                                (proc loco))) locos))

    (define (start-a-loco loco)
      (loco 'start)
      (set-loco-speed! (loco 'id) (loco 'speed)))

    (define (stop-a-loco loco)
      (set-loco-speed! (loco 'id) 0)
      (loco 'stop-loco))

    (define (invert-loco loco)
      (let ((speed (loco 'speed)))
        (loco 'invert!)
        (set-loco-speed! (loco 'id) (- speed))))

    (define (slow-down loco)
      (let ((speed (loco 'speed)))
        (if (positive? speed)
            (when (> speed 30)
              (loco 'slow!)
              (set-loco-speed! (loco 'id) (loco 'speed)))
            (when (< speed -30)
              (loco 'slow!)
              (set-loco-speed! (loco 'id) (loco 'speed))))))

    (define (unpause-loco! loco)
      (when (and (loco 'paused)
                 (not (eq? (loco 'seg) (loco 'dest)))) ;unpause only locos that arent at destination
        (when (and (free-path? (loco 'path)) ;path free = no reserved switches in path, nor locos in blocks in path
                   (not (zero? (loco 'speed)))) ;speed not zero = loco tried to start before but couldnt
          (start-a-loco loco))))

    ;;;FUNCTIONS FOR NMBS

    ;path is asked when loco is added
    (define (ask-path loco) ;asks path for loco via tcp to NMBS
      (let* ((dest (loco 'dest))
             (start (loco 'seg))
             (start-block (get-block start))
             (dest-block (get-block dest))
             (start-idx (number->string(start-block 'idx)))
             (dest-idx (number->string(dest-block 'idx)))
             (free-idx (number->string (find-free-loco-slot))))
        (sendmsg (vector "Path" start-idx dest-idx free-idx))))
 
    (define (add-loco! loco) ;;;
      (if (not (> loco-idx 2))
          (let* ((id (vector-ref loco 1))
                 (start-seg (vector-ref loco 2))
                 (prev-seg (vector-ref loco 3))
                 (dest (vector-ref loco 4)))
            (let* ((loco (make-train-adt id prev-seg start-seg dest))
                   (block (get-block (string->symbol start-seg)))
                   (loco-id (loco 'id))              
                   (start (loco 'seg))
                   (prev (loco 'last-pos)))
              (if (null? (block 'loco));is there a loco at chosen block?
                  (begin
                    (ask-path loco)
                    (add-loco loco-id prev start) ;simulator
                    ((block 'aloco) loco)
                    (vector-set! locos loco-idx loco)
                    (set! loco-idx (+ loco-idx 1)))
                  (displayln "this block is taken"))))
          (displayln "Cannot add more loco's")))                       

    (define (delete-loco! otp)
      (let* ((sel (string->number otp))
             (loco (vector-ref locos sel)))
        (when (not (null? loco))
          (let* ((pos-id (loco 'seg))
                 (loco-id (loco 'id))
                 (block (get-block pos-id))
                 (path (loco 'path)))
            (unreserve-switches! path)
            ((block 'uloco) '()) ;deletes the selected loco at block i
            (remove-loco loco-id) ;simulator
            (vector-set! locos sel '())
            (set! loco-idx (- loco-idx 1))
            (sendmsg (vector "Loco-data" "unselected" "" "" ""))))))

    (define (loco-speed! otp)
      (let* ((sel (vector-ref otp 1)))
        (if (eq? sel #f)
            (display "Please select a loco \n")
            (let* ((loco (vector-ref locos sel))
                   (loco-id (loco 'id)))
              (if (free-path? (loco 'path))
                (begin ((loco 'speed!) counter-clockwise)
                       (loco 'start)
                       (set-loco-speed! loco-id counter-clockwise) ;simulator
                       (send-loco-info sel)
                       (let* ((pos (loco 'seg))
                              (dest (loco 'dest))
                              (pos-bl (get-block pos))
                              (dest-bl (get-block dest))
                              (pos-idx (number->string (pos-bl 'idx)))
                              (dest-idx (number->string (dest-bl 'idx))))
                         ;send msg to ask again the edges of the current path
                         ;to ready the path
                         (sendmsg (vector "New-path!" (symbol->string loco-id) pos-idx dest-idx))))
                ((loco 'speed!) counter-clockwise))
              ))))

     
    ;;;INFRABEL FUNCTIONS

    (define (rdy-path edges) ;sets switches on the right state
      (for-each (lambda (edge) ;for each edge (u v label) ;label is state of switch
                  (let* ((state (car edge))
                         (from (string->number (cadr edge)))
                         (u (get-block-switch-by-idx from)))
                    (when (switch? (u 'id));because from u to v, u might be a detection block 
                      (when (not (u 'reserved))  ;if switch u is not reserved                               
                        (let ((id (u 'id))
                              (obj-state (u 'state)))
                          (cond ((and (string=? state "2");switch logica
                                      (= obj-state 1))
                                 (set-switch-position! id 2) ;simulator
                                 (reserve-switch! u));reserve the switch
                                ((and (string=? state "1")
                                      (= obj-state 2))
                                 (set-switch-position! id 1) ;simulator
                                 (reserve-switch! u))));reserve the switch
                        ))))
                edges))

    (define (free-path? path)
      (when (not (null? path))
        (let ((res #t))
          (for-each (lambda (element)
                      (when res
                        (let* ((from element)
                               (to-object (get-block-switch-by-idx from)))
                          (if (switch? (to-object 'id)) ;niet vrij als wissel gereserveerd is
                              (when (to-object 'reserved)
                                (set! res #f))
                              (when res ;enkel als pad nog vrij is
                                (let ((loco (to-object 'loco)))
                                  (when (not (null? loco)) ;niet vrij als er een loco bevindt op het pad
                                    (set! res #f))))))))
                    (cdr path))
          res)))

    (define (reserve-switch! switch)
      (switch 'reserve!)
      (switch 'switch!))

    (define contains member)
    
    (define (intersection a b) ;https://stackoverflow.com/questions/28550377/racket-intersect-two-lists/28750451
      (if (null? a)
          '()
          (if (not (contains (car a) b)) ;is in fact (not intersection)
              (cons (car a) (intersection (cdr a) b))
              (intersection (cdr a) b))))

    (define (update-loco-position loco)
      (let* ((loco-id (loco 'id))
             (new-pos (get-loco-detection-block loco-id))
             (loco-pos (loco 'seg))
             (prev-pos (loco 'last-pos))
             (pos-bl (get-block loco-pos))
             (prev-bl (get-block prev-pos))
             (dest-bl (get-block (loco 'dest)))
             (pos-idx (pos-bl 'idx))
             (dest-idx (dest-bl 'idx))
             (prev-idx (prev-bl 'idx)))
        (if (eq? new-pos prev-pos) ;helps to go in the right direction in loop mode
            (ask-path loco)          
            (when (and (not (eq? loco-pos new-pos))
                       (not (eq? new-pos #f))) ;ignores new-position if loco is undetectable in simulator
              ((loco 'pos!) new-pos)
              (send-loco-info sel))  ;send msg to nmbs with updated loco data
            )))

    (define (free-block? block)
      (null? (block 'loco)))

    (define (path-tracking loco) ;path finding + position update
      (let ((loco-pos (loco 'seg))
            (paused (loco 'paused)))
        (when (and (not (eq? loco-pos #f)) ;ignores loco if it is impossible to locate its position
                   (not paused))
          (let ((loco-pos-block (get-block loco-pos))
                (loco-path (loco 'path)))
            ;;updates position of each loco                                       
            (update-loco-position loco)
            ;;checks if loco is on right path
            (let* ((block-idx (loco-pos-block 'idx)))
              (when (eq? (member block-idx loco-path) #f) ;is the new position in the current path?
                (let* ((pos (loco 'seg))
                       (dest (loco 'dest))
                       (pos-block (get-block pos))
                       (dest-block (get-block dest))
                       (pos-idx (pos-block 'idx))
                       (dest-idx (dest-block 'idx)))
                  (sendmsg (vector "New-path!"
                                   (symbol->string (loco 'id))
                                   (number->string pos-idx)
                                   (number->string dest-idx))))
                )
              void
              )
            (let ((last-block (get-block (loco 'last-pos)))
                  (new-pos-block (get-block (loco 'seg))))
              ((last-block 'aloco) '());removes loco from last position
              ((new-pos-block 'aloco) loco)) ;add the loco at new block x                 
            ;;checks if loco is at destination
            (when (equal? (loco 'dest) loco-pos)
              (let* ((dest-block (get-block loco-pos))
                     (dest-block-idx (dest-block 'idx))
                     (dest (member dest-block-idx (loco 'path)))
                     (switches (intersection (loco 'path) dest)))
                (unreserve-switches! switches)
                ((loco 'path!) '())
                (stop-a-loco loco))) ;clears the loco in last visited block     
            ))))

    (define (unreserve-switches! switches)
      (when (not (null? switches))
        (for-each (lambda (switch-idx)
                    (let ((switch (get-block-switch-by-idx switch-idx)))
                      (when (switch? (switch 'id))
                        (switch 'unreserve!))))
                  switches)))
    
    ;;; Path checking for each loco
    (define (update-locos-and-track);;;
      (for-each-loco (lambda (loco)
                       (when (not (loco 'paused))
                         (path-tracking loco))
                       (unpause-loco! loco);start paused loco when path is "free"
                       )))
    

    ;;;RAILWAY SETUP INITIALIZATION

    (define (load-straight-setup)
      ;add all blocks & switch in buffer
      (let* ((bl (get-detection-block-ids))
             (blsiz (length bl))
             (v (make-vector (+ blsiz 1) '()))
             (idx 0))
        (for-each (lambda (blck)
                    (let* ((block (make-block-adt blck idx))
                           (node (mcons idx block)))
                      (vector-set! v idx block)
                      (insert! tree node))
                    (set! idx (+ idx 1)))
                  bl)
        (let* ((switch (make-switch-adt (car (get-switch-ids)) idx))
               (nodesw (mcons idx switch)))
          (insert! tree nodesw)
          (vector-set! v idx switch))
        (set! blocks v)))


    (define (setup-loop)
      ;add all blocks & switch in buffer
      (let* ((bl (get-detection-block-ids))
             (blsiz (length bl))
             (sw (get-switch-ids))
             (swsiz (length sw))
             (v (make-vector (+ blsiz swsiz) '()))
             (idx 0))
        (for-each (lambda (blck)
                    (let* ((bucket (vector-ref v idx))
                           (block (make-block-adt blck idx))
                           (node (mcons idx block)))
                      (vector-set! v idx block)
                      (insert! tree node))
                    (set! idx (+ idx 1)))
                  bl)
        (for-each (lambda (swtch)
                    (let* ((bucket (vector-ref v idx))
                           (sw-adt (make-switch-adt swtch idx))
                           (node (mcons idx sw-adt)))
                      (vector-set! v idx sw-adt)
                      (insert! tree node))
                    (set! idx (+ idx 1)))
                  sw)
        (set! blocks v))
      )

    (define (load-harware-setup);hardware setup
      (let ((sw (get-switch-ids))
            (bl (get-detection-block-ids))
            (otp '()))
        (set! switches (make-vector 10 '()))
        (for-each (lambda (sw) ;for each switch
                    (let* ((strr (symbol->string sw))
                           (str (if (= (string-length strr) 4)
                                    (substring strr 2 4)
                                    (substring strr 2 3)))
                           (id (string->number str))
                           (mod (modulo id 10))
                           (bucket (vector-ref switches mod))
                           (switch (if (= (string-length strr) 5)
                                       (make-switch-adt (string->symbol (substring strr 0 3)) id)
                                       (make-switch-adt sw id)))                                                
                           (node (mcons (switch 'idx) switch))) ;mutable conscel
                      (vector-set! switches mod (cons switch bucket));bucket is last digit
                      (insert! tree node);add the node in the avl-tree
                      (when (= (string-length strr) 5);for special switches such as S-2-3
                        (let* ((idd-str (substring strr 4 5));string operation to extract the values
                               (idd (string->number idd-str))
                               (modd (modulo idd 10))
                               (buck (vector-ref switches modd))
                               (swtch (make-switch-adt (string->symbol (string-append "S-" idd-str))
                                                       idd))
                               (nod (mcons (swtch 'idx) swtch)))
                          (vector-set! switches modd (cons swtch buck));add switch in bucket
                          (insert! tree nod)));insert avl
                      ))
                  sw)

        ;list of detection-block indexes
        (define l '(13 14 15 17 18 19 21 22 29 30 31 32 33 34 35 36))

        (when (not (null? l)) ;detection blocks
          (set! blocks (make-vector 10 '()))
          (for-each (lambda (bl) ; for each block
                      (let* ((strr (symbol->string bl))
                             (str (substring strr 2))
                             (id (string->number str))
                             (mod (modulo id 10))
                             (bucket (vector-ref blocks mod))
                             (block (make-block-adt bl (car l)))
                             (node (mcons (block 'idx) block)))
                        (set! l (cdr l))
                        (vector-set! blocks mod (cons block bucket));add block in bucket
                        (insert! tree node);insert avl
                        (set! otp (cons strr otp))))
                    bl))
        (sendmsg (vector "load-hardware" otp))
        ))

   
    (define (simulator msg)
      (cond ((string=? msg "Setup-hardware")
             (set! mode 0) 
             (set! counter-clockwise 120) ;speed vary in function of type of railway for better consistency
             (setup-hardware)
             (load-harware-setup));load every railway components in buffer
            ((string=? msg "Setup-straight-with-switch")
             (setup-straight-with-switch);load every loop railway components in buffer
             (set! mode 1)
             (set! counter-clockwise -60) 
             (set! switches '())
             (load-straight-setup) ;load straight-line components in buffer
             (let ((bs (segments)))
               (sendmsg (vector "load-straight-setup" bs))))
            ((string=? msg "Setup-loop-and-switches")
             (setup-loop-and-switches)
             (set! mode 2)
             (set! counter-clockwise 60)                    
             (set! switches '())
             (setup-loop)
             (let ((bs (segments)))
               (sendmsg (vector "load-loop-setup" bs))))
            ))

    (define (segments)
      (let ((res '())
            (lb '())
            (blocks (get-detection-block-ids))
            (switchez (get-switch-ids)))
        (for-each (lambda (block)
                    (set! lb (cons (symbol->string block) lb)))
                  blocks)
        (set! res lb)
        res
        ))

    (define (find-loco id)
      (let ((loco '()))
        (for-each-loco (lambda (loc)
                         (when (not (null? loc))
                           (when (equal? (loc 'id) id)
                             (set! loco loc)))))
        loco))

    (define (find-free-loco-slot)
      (let ((res '())
            (idx 0))
        (vector-map (lambda (loco)
                      (if (null? loco)
                          (set! res idx)
                          (set! idx (+ idx 1))))
                    locos)
        res))

    (define (reset-mode)
      (define l '(13 14 15 17 18 19 21 22 29 30 31 32 33 34 35 36))
      (stop);stop simulator
      (set! switches (make-vector 10 '()))
      (set! blocks (make-vector 10 '()))
      (set! locos (make-vector 3 '()))
      (set! loco-idx 0)
      (set! sel 0)
      (sendmsg "Reset"))

    ;;SERVER FUNCTIONS
    (define (sendmsg msg) ;send tcp msg
      ((server 'send) msg))

    (define (close-connection);close connection
      (server 'close))

    (define (send-loco-info loco-idx);send loco data to update in nmbs
      (let ((loco (vector-ref locos loco-idx)))
        (when (not (null? loco))
          (let ((loco-id (symbol->string (loco 'id)))
                (pos (symbol->string(loco 'seg)))
                (dest (symbol->string(loco 'dest)))
                (speed (number->string (loco 'speed))))
            (sendmsg (vector "Loco-data" loco-id pos dest speed))))))


    (define (same-path? p1 p2)
      (and (= (length p1) (length p2))
           (= (car p1) (car p2))))

    ;loop that checks tcp output     
    (define (check-otp)
      (let ((otp (server 'otp)))
        (when (not (null? otp))
          (if (vector? otp)
              (let ((msg (vector-ref otp 0)))
                (cond ((string=? msg "Add-loco")
                       (add-loco! otp))
                      ((string=? msg "Delete-loco")
                       (delete-loco! (vector-ref otp 1)))
                      ((string=? msg "Mode")
                       (simulator (vector-ref otp 1)))
                      ((string=? msg "Speed!")
                       (loco-speed! otp))
                      ((string=? msg "Invert!")
                       (invert-loco (vector-ref locos (vector-ref otp 1))))
                      ((string=? msg "Loco-info")
                       (let* ((idx (string->number (vector-ref otp 1)))
                              (loco (vector-ref locos idx)))
                         (when (not (null? loco))
                           (set! sel idx))
                         (send-loco-info idx)))
                      ((string=? msg "Path")
                       (let* ((path (vector-ref otp 1))
                                      
                              (slot (string->number (vector-ref otp 3)))
                              (loco (vector-ref locos slot)))
                         (when (not (null? loco))
                           (let* ((p (map (lambda (el)
                                            (string->number el)) path)))
                             ((loco 'path!) p)))))
                      ((string=? msg "New-Path!")
                       (let* ((loco-id (string->symbol(vector-ref otp 1)))
                              (path (vector-ref otp 2))
                              (loco (find-loco loco-id))
                              (p (map (lambda (el)
                                        (string->number el)) path)))
                         (let* ((otplen (vector-length otp))
                                (oldp (loco 'path))
                                (plen (length p))
                                (oldplen (length oldp))
                                (edges (vector-ref otp 3))
                                (free-path? (free-path? (loco 'path)))
                                (switches (intersection oldp p)))
                           (when free-path? ;only when path is free i.e. no reserved switches & no locos in the path
                             (stop-a-loco loco)
                             (rdy-path edges); set switches on the right state
                             (start-a-loco loco))
                           (cond ((= otplen 5) ;if prevpos = newpos && new-path < oldpath
                                  (if (< plen oldp)
                                      ((loco 'path!) p) ;update path
                                      (invert-loco loco)))
                                 ((= mode 2) ;loop mode
                                  (when (<= plen oldplen)
                                    (when (not free-path?)
                                      (stop-a-loco loco)
                                      (slow-down loco)
                                      (unreserve-switches! oldp)
                                      ((loco 'path!) p)
                                      (rdy-path edges)
                                      (start-a-loco loco))))
                                 ((= mode 0)
                                  (if (and (>= plen 8) ;in hardware mode length of loop is at least > 8
                                           (= plen oldplen))
                                      (when (not free-path?)
                                        ((loco 'path!) p) ;update path
                                        (unreserve-switches! switches)
                                        (rdy-path edges)
                                        )
                                      (let* ((prev-id (loco 'last-pos))
                                             (prev-bl (get-block prev-id))
                                             (prev-idx (prev-bl 'idx)))
                                        (cond ((same-path? p oldp)
                                               (stop-a-loco loco)
                                               (when (not free-path?)
                                                 (unreserve-switches! oldp)
                                                 (rdy-path edges))
                                               (start-a-loco loco))
                                              ((<= plen oldplen)
                                               (stop-a-loco loco)
                                               (slow-down loco) ;trein vertraagt telkens het pad korter wordt
                                               ((loco 'path!) p)
                                               (start-a-loco loco)
                                                 (invert-loco loco)
                                                 (send-loco-info sel)
                                               (unreserve-switches! switches)))
                                              )))
                                 ))))
                      ))
              (cond ((string=? otp "Stop")
                     (reset-mode))
                    ((string=? otp "Start")
                     (start))
                    ((string=? otp "Exit")
                     (exit)
                     )
                    ((string=? otp "TCP-test")
                       (sendmsg (vector "TCP-test" "succes")))
                    ((string=? otp "close-tcp")
                     (close-connection))
                    ((eq? otp eof)
                     (exit))
                    )))))


    (define (loop)
      (thread
       (lambda ()
         (check-otp)
         (server 'otp-clean)
         (update-locos-and-track)
         (sleep 1/2)
         (loop))))
    
    (loop)
    
    (define (infrabel-dispatch m)
      (cond ((eq? m 'add-loco!) add-loco!)
            ((eq? m 'start) start)
            ((eq? m 'stop) stop)
            ((eq? m 'stop-loco) stop-a-loco)
            ((eq? m 'delete-loco!) delete-loco!)
            ((eq? m 'loco-speed!) loco-speed!)
            ((eq? m 'switch?) switch?)
            ((eq? m 'get) get-block-switch-by-idx)
            ((eq? m 'getb) get-block)
            ((eq? m 'locos) locos)
            ((eq? m 'sw) switches)
            ((eq? m 'free) free-path?)
            ((eq? m 'mode) simulator)
            ((eq? m 'send) sendmsg)
            ((eq? m 'close-connection) (close-connection))
            ((eq? m 's) server)
            ))
    infrabel-dispatch))