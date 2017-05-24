#lang typed/racket

(require typed/rackunit)
(require "../lib/graph.rkt") 

(: message_err String)
(define message_err "doesn't match")

(: g-empty Graph)
(define g-empty (cons 0 '()))

(: g-4 Graph)
(define g-4 (cons 4 (list (Vertex 0 '(1 2))
                        (Vertex 1 '(0 3))
                        (Vertex 2 '(0 3))
                        (Vertex 3 '(1 2)))))
(: g-not-connexe Graph)
(define g-not-connexe (cons 5 (list (Vertex 0 '(1 2))
                        (Vertex 1 '(0 3))
                        (Vertex 2 '(0 3))
                        (Vertex 3 '(1 2))
                        (Vertex 4 '()))))


(: test-graph (-> Any))
(define (test-graph)
  (begin
  (test-graph-size)
  (test-graph-list)
  (test-graph-neighbours)
  (test-graph-linked)))

(: test-graph-size (-> Any))
(define (test-graph-size)
  (begin
  (check-equal? (graph-size g-empty) 0 message_err)
  (check-equal? (graph-size g-4) 4 message_err)
  (check-equal? (graph-size g-not-connexe) 5 message_err)))

(: vertex-equal (-> Vertex Vertex Boolean))
(define (vertex-equal v l)
  (and (equal? (Vertex-id v) (Vertex-id l)) (equal? (Vertex-neigh v) (Vertex-neigh l))))

(: test-graph-list (-> Any))
(define (test-graph-list)
  (begin
  (check-equal? (graph-list g-empty) null message_err)
  (check-true (andmap vertex-equal (graph-list g-4)
                          (list (Vertex 0 '(1 2))
                                (Vertex 1 '(0 3))
                                (Vertex 2 '(0 3))
                                (Vertex 3 '(1 2))))))
  (check-false (andmap vertex-equal (graph-list g-4)
                       (list (Vertex 0 '(1 2))
                             (Vertex 1 '(0 3))
                             (Vertex 2 '(1 3))
                             (Vertex 3 '(1 2))))))

(: test-graph-neighbours (-> Any))
(define (test-graph-neighbours)
  (begin
  (check-not-equal? (graph-neighbours g-empty 0) '(1 2))
  (check-equal? (graph-neighbours g-4 0) '(1 2))
  (check-not-equal? (graph-neighbours g-4 0) '(0 3)))
  (check-equal? (graph-neighbours g-not-connexe 5) '())
  (check-equal? (graph-neighbours g-not-connexe 6) '()))

(: test-graph-linked (-> Any))
(define (test-graph-linked)
  (begin
    (check-true (graph-linked? g-empty 0 0))
    (check-true (graph-linked? g-4 0 3))
    (check-false (graph-linked? g-not-connexe 0 5))))