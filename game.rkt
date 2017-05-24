#lang typed/racket

(require "lib/graph.rkt")
(require "lib/integer-set.rkt")
(require "mines.rkt")

(provide num-neighbours-bombs reveal)
(: g Graph)
(define g (cons 4 (list (Vertex 0 '(1 2))
                        (Vertex 1 '(0 3))
                        (Vertex 2 '(0 3))
                        (Vertex 3 '(1 2)))))

(: num-member-bombs (-> Integer-set (Listof Integer) Integer))
(define (num-member-bombs bombs-map l)
  (cond ((null? l)
         0)
        ((have-bomb? bombs-map (car l))
         (+ 1 (num-member-bombs bombs-map (cdr l))))
        (else (num-member-bombs bombs-map (cdr l)))))
       

(: num-neighbours-bombs (-> Graph Integer-set Integer Integer))
(define (num-neighbours-bombs graph bombs-map n)
  (let ((l (graph-neighbours graph n)))
    (num-member-bombs bombs-map l)))


(: reveal (Graph Integer Integer-set Integer-set -> Integer-set))
(define (reveal g n b m)
  (: pred (Integer -> Boolean))
  (define (pred n)
    (not (is-in-set? m n)))
  (: iter (Integer-set (Listof Integer) -> Integer-set))
  (define (iter m neigh)
    (if (null? neigh)
        (add-in-set m n)
        (iter (reveal g (car neigh) b (add-in-set m n))
              (cdr neigh))))
  (cond ((have-bomb? b n) (add-in-set m n))
        ((is-in-set? m n)
         m)
        ((not (zero? (num-member-bombs b (graph-neighbours g n))))
         (add-in-set m n))
        (else
         (iter m (filter pred (graph-neighbours g n))))))
