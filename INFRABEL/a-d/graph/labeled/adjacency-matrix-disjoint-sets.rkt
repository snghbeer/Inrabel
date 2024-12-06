#lang r6rs

;; Algoritmen en datastructuren II (academiejaar 2011-2012)
;; Oplossingen voor oefeningenreeks 3: Grafen
;; Vraag 2 - Connectiviteit van grafen testen met disjoint sets


;; a) Dit kan door een disjoint-sets ADTs aan te maken met initieel evenveel singletons
;;    als er nodes in de graf zijn. Voor elke edge die toegevoegd wordt zoeken (find) we
;;    dan de deelverzameling waartoe de from node behoort en die waartoe de to node behoort
;;    vervolgens voegen we beide samen (union). Om dan na te gaan of de graf geconnecteerd
;;    is volstaat het om na te gaan hoeveel deelverzamelingen er zijn. Als er maar 1 is de
;;    de graf geconnecteerd, als er meerdere zijn niet (elke deelverzameling stelt dan 1 van
;;    de geconnecteerde componenten voor).

;; b) Connectiviteit nagaan met disjoint sets werkt niet voor gerichte grafen. De geconnecteerde
;;    nodes zitten immers in dezelfde verzameling wat ons geen informatie over de richting van
;;    de edge geeft.

;; c) & d) Zie hieronder. Aanpassingen zijn aangeven met "!!!"

;; e) Om connectiviteit met disjoint sets te doen werken mag je geen delete-edge! doen! Het is
;;    zeer onefficient (en daarbij ook lastig om te implementeren) om dit met Disjoint Sets te
;;    doen. Er moet immers voor elke edge die je verwijdert gecontroleerd worden of het partities
;;    in de geconnecteerde deelgraffen veroorzaakt. Voor elk van deze partities moeten de originele
;;    verzamelingen weer uit mekaar getrokken worden in nieuwe disjoint sets die de nieuwe
;;    geconnecteerde deelgraffen voorstellen na het verwijderen van de edge. Daarom zullen we
;;    (gemakshalve) delete-edge! onmogelijk maken in deze implementatie.

(library
 (unweighted-graph)
 (export new unweighted-graph? order nr-of-edges directed?
         for-each-node for-each-edge
         add-edge! delete-edge! 
         adjacent?
         connection? connected? ;!!!
         )
 (import (rnrs base)
         (srfi :9)
         (rnrs control)
         (rnrs mutable-pairs)
         (prefix (a-d disjoint-sets optimized+number-of-sets) ds:)  ;!!!
         )
 
  (define-record-type unweighted-graph
   (make d n s ds)
   unweighted-graph?
   (d directed?)
   (n nr-of-edges nr-of-edges!)
   (s storage)
   (ds disjoint-set)  ;!!!
   )
           
 (define (new directed order)
   (define (make-row i)
     (if directed
       (make-vector order #f)
       (make-vector (- i 1) #f)))
   (define rows (make-vector order))
   (make directed
         0 ; nr-of-edges
         (let for-all
           ((row 1))
           (vector-set! rows (- row 1) (make-row row))
           (if (< row order)
             (for-all (+ row 1))
             rows))
         (ds:new order) ;!!!
         ))
  
 (define (order graph)
   (vector-length (storage graph)))
 
 (define (for-each-node graph proc)
   (let for-all
     ((node 0))
     (proc node)
     (if (< (+ node 1) (order graph))
       (for-all (+ node 1))))
   graph)
 
 (define (for-each-edge graph from proc)
   (let for-all
     ((to 0))
     (if (adjacent? graph from to)
         (proc to))
     (if (< (+ to 1) (order graph))
         (for-all (+ to 1))))
   graph)
 
 (define (add-edge! graph from to)
   (define rows (storage graph))
   (define row-idx (if (directed? graph) from (max from to)))
   (define col-idx (if (directed? graph) to (min from to)))
   (define row (vector-ref rows row-idx))
   (define set (disjoint-set graph)) ;!!!
   (when (not (vector-ref row col-idx))
     (vector-set! row col-idx #t)
     (nr-of-edges! graph (+ 1 (nr-of-edges graph))))
   (ds:union! set (ds:find set from) (ds:find set to))  ;!!!
   graph)
 
 (define (delete-edge! graph from to)  ;!!! zie e)
   ;;(define rows (storage graph))
   ;;(define row-idx (if (directed? graph) from (max from to)))
   ;;(define col-idx (if (directed? graph) to (min from to)))
   ;;(define row (vector-ref rows row-idx))
   ;;(when (vector-ref row col-idx)
   ;;  (vector-set! row col-idx #f)
   ;;  (nr-of-edges! graph (- (nr-of-edges graph) 1)))
   ;;graph)
   (error "delete-edge! is not implemented"))

 (define (adjacent? graph from to)
   (define rows (storage graph))
   (if (directed? graph)
       (let ((row (vector-ref rows from)))
         (vector-ref row to))
       (and (not (= from to))
            (vector-ref (vector-ref rows (max from to)) 
                        (min from to)))))
 
  ;;c)
 (define (connection? graph from to)  ;!!!
   (define set (disjoint-set graph))
   (ds:same-set? (ds:find set from) (ds:find set to)))
 
 ;;d)
 (define (connected? graph)  ;!!!
   (define set (disjoint-set graph))
   (= (ds:number-of-sets set) 1))
 )
