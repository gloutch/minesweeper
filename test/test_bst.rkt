#lang typed/racket

(require "../lib/bst.rkt")
(require "../lib/tree.rkt")


(: lt (Lower-than Integer))
(define lt <)

(: tree (Tree Integer))
(define tree '((nil 0 nil) 1 (nil 3 nil)))

(: ex1 (Bst Integer))
(define ex1 (bst-empty lt))

(: ex2 (Bst Integer))
(define ex2 (Bst 3 lt tree))



; empty
(: test-empty (-> Boolean))
(define (test-empty)
  (and
    (eq? (bst-empty? ex1) #t)
    (eq? (bst-empty? ex2) #f)))

; ->list
(: test-list (-> Boolean))
(define (test-list)
  (and
   (equal? (bst->list ex1) '())
   (equal? (bst->list ex2) '(1 0 3))))

; size
(: test-size (-> Boolean))
(define (test-size)
  (and 
   (eq? (bst-size ex1) 0)
   (eq? (bst-size ex2) 3)  
   (eq? (bst-size (bst-add ex2 0)) 3)
   (eq? (bst-size (bst-add ex2 4)) 4)
   (eq? (bst-size (bst-remove ex2 2)) 3)
   (eq? (bst-size (bst-remove ex2 0)) 2)
   (eq? (bst-size (bst-union ex1 ex1)) 0)
   (eq? (bst-size (bst-union ex1 ex2)) 3)
   (eq? (bst-size (bst-union ex2 ex2)) (bst-size ex2))))

; height
(: test-height (-> Boolean))
(define (test-height)
  (and 
   (eq? (bst-height ex1) 0)
   (eq? (bst-height ex2) 2)))

; min
(: test-min (-> Boolean))
(define (test-min)
  (and 
   (eq? (bst-min ex2) 0)))

; max
(: test-max (-> Boolean))
(define (test-max)
  (and 
   (eq? (bst-size ex2) 3)))

; find
(: test-find (-> Boolean))
(define (test-find)
  (and 
   (eq? (bst-find ex1 0) #f)
   (eq? (bst-find ex2 1) #t)
   (eq? (bst-find ex2 3) #t)
   (eq? (bst-find ex2 8) #f)))

;add
(define tmp1 (bst-add ex1 5))
(define tmp2 (bst-add tmp1 4))
(define ex3 (bst-add tmp2 6))

(: test-add (-> Boolean))
(define (test-add)
  (and
   (eq? (bst-find ex3 0) #f)
   (eq? (bst-find ex3 4) #t)
   (eq? (bst-find ex3 5) #t)
   (eq? (bst-find ex3 6) #t)))

;union
(define uni (bst-union ex2 ex3))

(: test-union (-> Boolean))
(define (test-union)
  (and
   (eq? (bst-size uni) (+ (bst-size ex2) (bst-size ex3)))
   (eq? (bst-find uni 0) #t) ;in ex2
   (eq? (bst-find uni 1) #t)
   (eq? (bst-find uni 3) #t)
   (eq? (bst-find uni 4) #t) ;in ex3
   (eq? (bst-find uni 5) #t)
   (eq? (bst-find uni 6) #t)))

;remove
(define uni1 (bst-remove uni 0))
(define uni2 (bst-remove uni1 1))
(define ex4  (bst-remove uni2 6))

(: test-remove (-> Boolean))
(define (test-remove)
  (and
   (eq? (bst-find ex4 0) #f) ;in ex2
   (eq? (bst-find ex4 1) #f)
   (eq? (bst-find ex4 3) #t)
   (eq? (bst-find ex4 4) #t) ;in ex3
   (eq? (bst-find ex4 5) #t)
   (eq? (bst-find ex4 6) #f)
   (eq? (bst-size ex4) (- (bst-size uni) 3))))


;main
(and
 (test-empty)
 (test-list)
 (test-size)
 (test-height)
 (test-min)
 (test-max)
 (test-find)
 (test-add)
 (test-union)
 (test-remove))








