#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*    Depth First Traversal (version: labeled; and unweighted)     *-*-
;-*-*                                                                 *-*-
;-*-*                        Matthias Stevens                         *-*-
;-*-*                 2009-2011 Software Languages Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (dft-labeled)
 (export dft node-nop root-nop edge-nop)
 (import (rnrs base)
         (rnrs control)
         (a-d graph labeled config))
 
 (define (dft graph
              root-discovered
              node-discovered
              node-processed
              edge-discovered
              edge-processed
              edge-bumped
              . roots)
   (define visited (make-vector (order graph) #f))
   (define exit '())
   (define (dft-component root root-label)
     (define (dft-rec from)
       (unless (node-discovered from (label graph from))
         (exit #f))
       (vector-set! visited from #t)
       (for-each-edge
        graph
        from
        (lambda (to edge-label)
          (if (vector-ref visited to)
              (unless (edge-bumped from to edge-label)
                (exit #f))
              (unless (and (edge-discovered from to edge-label)
                           (dft-rec to)
                           (edge-processed from to edge-label))
                (exit #f)))))
       (unless (node-processed from (label graph from))
         (exit from)))
     (when (not (vector-ref visited root))
       (if (root-discovered root (label graph root))
           (dft-rec root)
           (exit #f))))
   (call-with-current-continuation
    (lambda (cont)
      (set! exit cont)
      (if (null? roots)
          (for-each-node graph dft-component)
          (for-each
           (lambda (root)
             (dft-component root (label graph root)))     
           (car roots))))))

 (define (root-nop root labl) #t)
 (define (node-nop node labl) #t)
 (define (edge-nop from to labl) #t))