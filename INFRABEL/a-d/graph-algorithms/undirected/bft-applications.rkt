#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Undirected BFS Applications                    *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (basic algorithms)
 (export shortest-path neighbors)
 (import (rnrs base)
         (a-d graph labeled config)
         (a-d graph-traversing bft-labeled))
 
 (define (shortest-path g from to)
   (define paths (make-vector (order g) '()))
   (vector-set! paths from (list from))
   (bft g 
        root-nop
        (lambda (node label) 
          (not (eq? node to)))
        (lambda (from to label)
          (vector-set! paths to (cons to (vector-ref paths from))))
        edge-nop
        (list from))
   (vector-ref paths to))

 (define (neighbors g v) ;;added
   (define paths '())
   (bft g 
        root-nop
        (lambda (node label) 
          (if (or (adjacent? g node v)
                  (adjacent? g v node))
              (begin (set! paths (cons node paths)))))
        edge-nop
        edge-nop
        (list v))
   paths)
)