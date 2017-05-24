#lang typed/racket

(require "tree.rkt")
;(provide (all-defined-out))
(provide Bst
         Lower-than
         bst-empty
         bst-empty?
         bst-eq?
         bst-size
         bst-height
         bst-min
         bst-max
         bst-find
         bst-add
         bst-union
         bst-remove
         bst->list)         

(define-type (Lower-than A) (-> A A Boolean))
(struct: (A) Bst ([size : Integer]
                  [lt   : (Lower-than A)]
                  [tree : (Tree A)]))



(: bst-empty (All (A) (-> (Lower-than A) (Bst A))))
(define (bst-empty lt)
  (Bst 0 lt 'nil))



(: bst-empty? (All (A) (-> (Bst A) Boolean)))
(define (bst-empty? b)
  (and (zero? (Bst-size b))
       (tree-empty? (Bst-tree b))))



(: bst-eq? (All (A) (-> (Bst A) (Bst A) Boolean)))
(define (bst-eq? b1 b2)
  (and (eq?      (Bst-size b1) (Bst-size b2))
       (tree-eq? (Bst-tree b1) (Bst-tree b2))))


(: bst->list (All (A) (-> (Bst A) (Listof A))))
(define (bst->list t)
  (tree->list (Bst-tree t)))


(: bst-size (All (A) (-> (Bst A) Integer)))
(define bst-size Bst-size)


(: bst-height (All (A) (-> (Bst A) Integer)))
(define (bst-height b)
  (tree-height (Bst-tree b)))



(: bst-min (All (A) (-> (Bst A) A)))
(define (bst-min b)
  (tree-leftmost (Bst-tree b)))



(: bst-max (All (A) (-> (Bst A) A)))
(define (bst-max b)
  (tree-rightmost (Bst-tree b)))


         
(: bst-find (All (A) (-> (Bst A) A Boolean)))
(define (bst-find b v)
  (bst-find-tree (Bst-lt b) (Bst-tree b) v))

(: bst-find-tree (All (A) (-> (Lower-than A) (Tree A) A Boolean)))
(define (bst-find-tree lt t v)
  (match t
    ['nil #f]
    [(list ls n rs)
     (cond [(lt n v) (bst-find-tree lt rs v)]
           [(lt v n) (bst-find-tree lt ls v)]
           [else #t])])); neither lower and higher -> it's equal



(: bst-add (All (A) (-> (Bst A) A (Bst A))))
(define (bst-add b v)
  (if (bst-find b v)
      b
      (Bst
       (add1 (Bst-size b))
       (Bst-lt b)
       (bst-add-tree (Bst-lt b) (Bst-tree b) v))))

(: bst-add-tree (All (A) (-> (Lower-than A) (Tree A) A (Tree A))))
(define (bst-add-tree lt t v) 
  (match t
    ['nil (list 'nil v 'nil)]
    [(list ls n rs)
     (cond [(lt n v) (list ls n (bst-add-tree lt rs v))]
           [(lt v n) (list (bst-add-tree lt ls v) n rs)]
           [else t])])) ; v is already in t



; with the same (lt) function
; better with (Bst-size b1) > (Bst-size b2)
(: bst-union (All (A) (-> (Bst A) (Bst A) (Bst A))))
(define (bst-union b1 b2)
  (let ([tree (bst-union-tree (Bst-lt b1) (Bst-tree b1) (Bst-tree b2))])
    (Bst
     (tree-size tree)
     (Bst-lt b1)
     tree)))

(: bst-union-tree (All (A) (-> (Lower-than A) (Tree A) (Tree A) (Tree A))))
(define (bst-union-tree lt t1 t2)
  (match t2
    ['nil t1]
    [(list ls n rs)
     (bst-union-tree lt (bst-union-tree lt (bst-add-tree lt t1 n) rs) ls)]))



(: bst-remove (All (A) (-> (Bst A) A (Bst A))))
(define (bst-remove b v)
  (if (bst-find b v)
      (Bst
       (sub1 (Bst-size b))
       (Bst-lt b)
       (bst-remove-tree (Bst-lt b) (Bst-tree b) v))
      b))

(: bst-remove-tree (All (A) (-> (Lower-than A) (Tree A) A (Tree A))))
(define (bst-remove-tree lt t v)
  (match t
    ['nil (raise "not in the tree")]
    [(list ls n rs)
     (cond [(lt n v) (list ls n (bst-remove-tree lt rs v))]
           [(lt v n) (list (bst-remove-tree lt ls v) n rs)]
           [else (bst-remove-root t)])]))

(: bst-remove-root (All (A) (-> (Tree A) (Tree A))))
(define (bst-remove-root t)
  (match t
    [(list 'nil n 'nil) 'nil]
    [(list 'nil n rs  ) rs  ]
    [(list ls   n 'nil) ls  ]
    [(list ls   n rs  ) (list ls (tree-leftmost rs) (bst-remove-min rs))])) 

(: bst-remove-min (All (A) (-> (Tree A) (Tree A))))
(define (bst-remove-min t)
  (match t
    ['nil (raise "remove-min in an empty tree")]
    [(list 'nil n rs) rs]
    [(list ls n rs) (list (bst-remove-min ls) n rs)]))