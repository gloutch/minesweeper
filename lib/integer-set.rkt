#lang typed/racket

(require "bst.rkt" )

(provide Integer-set
         integer-set-size
         is-in-set?
         add-in-set
         empty-integer-set
         integer-set-remove
         list->integer-set
         integer-set->list
         boolean-list->integer-set)


(: int-lt (Lower-than Integer))
(define int-lt <)

(define-type Integer-set (Bst Integer))

( : integer-set-size (-> Integer-set Integer))
(define integer-set-size bst-size)

(: empty-integer-set (-> Integer-set))
(define (empty-integer-set)
  (bst-empty int-lt))

(: is-in-set? (-> Integer-set Integer Boolean))
(define (is-in-set? set number)
  (bst-find set number))

(: add-in-set (-> Integer-set Integer Integer-set))
(define (add-in-set set number)
  (bst-add set number))

(: integer-set-remove (-> Integer-set Integer Integer-set))
(define (integer-set-remove set number)
  (bst-remove set number))

(: list->integer-set (-> (Listof Integer) Integer-set))
(define (list->integer-set l)
  (if (null? l)
      (empty-integer-set)
      (add-in-set (list->integer-set (cdr l)) (car l))))

( : integer-set->list (-> Integer-set (Listof Integer)))
(define integer-set->list bst->list)


( : boolean-list->integer-set (-> (Listof Boolean) Integer-set))
(define (boolean-list->integer-set l)
  (: aux (-> (Listof Boolean) Integer Integer-set))
  (define (aux l offset)
    (cond
      [(null? l) (empty-integer-set)]
      [(car l) (add-in-set
                (aux (cdr l) (add1 offset))
                offset)]
      [else (aux (cdr l) (add1 offset))]))
  (aux l 0))
