#lang r6rs

;; Algoritmen en datastructuren II (academiejaar 2011-2012)
;; Oplossingen voor oefeningenreeks 3: Grafen
;; Vraag 1 - Gelabelde grafen gerepresenteerd als adjacency lists


;; a) Zie hieronder.

;; b) Net zoals bij ongelabelde grafen is de adjacency list representatie voordeliger wanneer we te maken hebben
;;    met ijle grafen. Het nadeel t.o.v. de gelabelde graf implementatie met een adjacency matrix is dat het
;;    opvragen van de label van een edge in O(|V|) gebeurt, terwijl dat met de matrix in O(1) kan.

(library
 (labeled-graph)
 (export new labeled-graph? order directed? nr-of-edges 
         for-each-node for-each-edge
         add-edge! delete-edge!
         adjacent? 
         label label! edge-label) ;; !!!
 (import (srfi :9)
         (rnrs base (6))
         (rnrs mutable-pairs))
 
 (define-record-type labeled-graph
   (make d nr s nl)
   labeled-graph?
   (d directed?)
   (nr nr-of-edges nr-of-edges!)
   (s storage storage!)
   (nl node-labels node-labels!)) ;; !!!

 ;; Edge is een cons-cell van label en to
 (define make-graph-edge cons)
 (define graph-edge-label car)
 (define graph-edge-to cdr)
 
 (define (new directed nr-of-nodes)
   (make directed
         0
         (make-vector nr-of-nodes '())
         (make-vector nr-of-nodes 'no-label)))
 
 (define (order graph)
   (vector-length (storage graph)))

 ;; Nieuwe functies label en label!
 (define (label graph node)
   (vector-ref (node-labels graph) node))
 
 (define (label! graph node label)
   (vector-set! (node-labels graph) node label))

 (define (for-each-node graph proc)
   (let for-all
     ((node 0))
     (proc node (label graph node)) ;; !!!
     (if (< (+ node 1) (order graph))
         (for-all (+ node 1))))
   graph)

 (define (for-each-edge graph from proc)
   (define row (vector-ref (storage graph) from))
   (let iter-edges
     ((edges row))
     (if (not (null? edges))
         (let ((edge (car edges)))
           (proc (graph-edge-to edge) (graph-edge-label edge)) ;; !!!
           (iter-edges (cdr edges)))))
   graph)
 
 (define (add-edge! graph from to label)
   (define lists (storage graph))
   (define edge (make-graph-edge label to)) ;; !!!
   (define (insert-sorted edge prev next! next)
     (cond 
       ((or (null? next)
            (> (graph-edge-to edge) (graph-edge-to (car next))))
        (next! prev (cons edge next))
        #t)
       ((= (graph-edge-to edge) (graph-edge-to (car next)))
        #f)
       (else
        (insert-sorted edge next set-cdr! (cdr next)))))
   (define (head-setter head) 
     (lambda (ignore next)
       (vector-set! lists head next)))
   (if (insert-sorted edge '() (head-setter from) (vector-ref lists from))
       (nr-of-edges! graph (+ 1 (nr-of-edges graph))))
   (if (not (directed? graph))
       (let ((reverse (make-graph-edge label from))) ;; !!!
         (insert-sorted reverse '() (head-setter to) (vector-ref lists to))))
   graph)
 
 (define (delete-edge! graph from to)
   (define lists (storage graph))
   (define (delete-sorted to prev next! next)
     (cond 
       ((or (null? next)
            (> to (graph-edge-to (car next))))
        #f)
       ((= to (graph-edge-to (car next)))
        (next! prev (cdr next))
        #t)
       (else
        (delete-sorted to next set-cdr! (cdr next)))))
   (define (head-setter head) 
     (lambda (ignore next)
       (vector-set! lists head next)))
   (if (delete-sorted to '() (head-setter from) (vector-ref lists from))
       (nr-of-edges! graph (- (nr-of-edges graph) 1)))
   (if (not (directed? graph))
       (delete-sorted from '() (head-setter to) (vector-ref lists to)))
   graph)
 
 (define (adjacent? graph from to)
   (define lists (storage graph))
   (let search-sorted
     ((curr (vector-ref lists from)))
     (cond 
       ((or (null? curr)
            (< (graph-edge-to (car curr)) to))
        #f)
       ((= (graph-edge-to (car curr)) to)
        #t)
       (else
        (search-sorted (cdr curr))))))

 ;; Alternatieve versie met dezelfde performantie
 ;; Geeft ofwel #f terug ofwel een symbol
 ;; wat #t is in bv. een if-test
 ;(define (adjacent? graph from to)
 ;  (edge-label graph from to))

 (define (edge-label graph from to)  ;O(|V|), zie b)
   (define (find-edge-label row)
     (cond ((or (null? row)
                (< (graph-edge-to (car row)) to))
            #f)
           ((= (graph-edge-to (car row)) to)
            (graph-edge-label (car row)))
           (else
            (find-edge-label (cdr row)))))
   (find-edge-label (vector-ref (storage graph) from)))
 
 )
