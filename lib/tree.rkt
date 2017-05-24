#lang typed/racket
(provide (all-defined-out))


(define-type (Tree A) (U  'nil  (List (Tree A) A (Tree A))))



(: tree-empty? (All (A) (-> (Tree A) Boolean)))
(define (tree-empty? t)
  (eq? t 'nil))


(: tree->list (All (A) (-> (Tree A) (Listof A))))
(define (tree->list t)
  (match t
    ['nil null]
    [(list ls n rs)
     (cons n (append (tree->list ls) (tree->list rs)))]))


(: tree-eq? (All (A) (-> (Tree A) (Tree A) Boolean)))
(define (tree-eq? t1 t2)
  (match (list t1 t2)
    [(list 'nil 'nil) #t]
    [(list (list ls1 n1 rs1) (list ls2 n2 rs2))
     (and (eq? n1 n2)
          (tree-eq? ls1 ls2)
          (tree-eq? rs1 rs2))]
    [else #f]))


(: tree-construct (All (A) (-> (Tree A) A (Tree A) (Tree A))))
(define (tree-construct ls n rs)
  (list ls n rs))


(: tree-size (All (A) (-> (Tree A) Integer)))
(define (tree-size t)
  (match t
    ['nil 0]
    [(list ls _ rs) (+ 1 (tree-size ls) (tree-size rs))]))


(: tree-height (All (A) (-> (Tree A) Integer)))
(define (tree-height t)
  (match t
    ['nil 0]
    [(list ls _ rs) (+ 1 (max (tree-height ls) (tree-height rs)))]))


(: tree-leftmost (All (A) (-> (Tree A) A)))
(define (tree-leftmost t)
  (match t
    ['nil (raise "Looking for element in empty Tree")]
    [(list 'nil n _) n]
    [(list  ls  _ _) (tree-leftmost ls)]))


(: tree-rightmost (All (A) (-> (Tree A) A)))
(define (tree-rightmost t)
  (match t
    ['nil (raise "Looking for element in empty Tree")]
    [(list _ n 'nil) n]
    [(list _ _  rs) (tree-rightmost rs)]))