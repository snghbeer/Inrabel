#lang r6rs

;; Algoritmen en datastructuren II (academiejaar 2011-2012)
;; Oplossingen voor oefeningenreeks 3: Grafen
;; Vraag 2 - Connectiviteit van grafen testen met disjoint sets


;; Kleine aanpassing van "Disjoint Sets (Optimized Implementation)" (a-d\disjoint-sets\optimized.rkt)
;;   om het aantal disjoint sets te kunnen opvragen (nodig voor e)).

(library
 (disjoint-sets)
 (export new disjoint-sets? find union! same-set?
         number-of-sets ;!!!
         )
 (import (srfi :9)
         (rnrs base)
         (rnrs control))
 
 (define-record-type disjoint-sets
   (make t r)
   disjoint-sets?
   (t up-trees)
   (r tree-ranks)
   (ns number-of-sets number-of-sets!) ;!!!
   )
 
 (define (new size)
   (define trees (make-vector size 0))
   (define ranks (make-vector size 0))
   (define sets (make trees ranks))
   (number-of-sets! sets size) ;!!!
   (let fill-singletons 
     ((i 0))
     (vector-set! trees i i)
     (if (< (+ 1 i) size)
       (fill-singletons (+ i 1))))
   sets)
 
 (define same-set? =)
 
 (define (find sets nmbr)
   (define trees (up-trees sets))
   (define (up-tree-rec elmt)
     (if (not (eq? elmt (vector-ref trees elmt)))
       (vector-set! trees elmt (up-tree-rec (vector-ref trees elmt))))
     (vector-ref trees elmt))
   (up-tree-rec nmbr))
 
 (define (union! sets set1 set2)
   (define ranks (tree-ranks sets))
   (define trees (up-trees sets))
   (when (not (same-set? set1 set2))  ;!!!
     (number-of-sets! sets (- (number-of-sets sets) 1))  ;!!!
     (cond ((> (vector-ref ranks set1) 
               (vector-ref ranks set2))
            (vector-set! trees set2 set1))
           ((= (vector-ref ranks set1) 
               (vector-ref ranks set2))
            (vector-set! trees set1 set2)
            (vector-set! ranks set2 (+ 1 (vector-ref ranks set2))))
           (else
            (vector-set! trees set1 set2))))
     sets))