#lang typed/racket
(provide (all-defined-out))

;(define-type Graph (Pairof Integer (Listof (Pairof Integer (Pairof (Listof Integer) '())))))
(define-type Graph (Pairof Integer (Listof Vertex)))
(struct: Vertex ([id : Integer ] [neigh : (Listof Integer) ]))



(: graph-size (-> Graph Integer))
(define (graph-size g)
  (car g))


(: graph-list (-> Graph (Listof Vertex)))
(define (graph-list g)
  (cdr g))


(: graph-neighbours (-> Graph Integer (Listof Integer)))
(define (graph-neighbours g n)
  (: p (Vertex -> Boolean))
  (define (p v) (eq? (Vertex-id v) n))
  (let ([vertex  (filter p (graph-list g))])
  (if (null? vertex)
      '()
  (Vertex-neigh (car vertex)))))


(: graph-linked? (Graph Integer Integer -> Boolean))
(define (graph-linked? g m n)
  (not ((inst member Integer Integer) m (graph-neighbours g n))))

; Does NOT return a list in the same format that is parsed, possibly unused
;(: print-graph (Graph -> (Listof (Listof Integer))))
;(define (print-graph g)
;  (: aux (Vertex -> (Listof Integer)))
;  (define (aux v)
;    (Vertex-neigh v))
;  (map aux (graph-list g)))