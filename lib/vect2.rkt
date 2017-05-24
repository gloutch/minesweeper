#lang typed/racket
(provide (all-defined-out))


(struct: vect2 ([x : Real] [y : Real]))


; add
(: vect2-add (-> vect2 vect2 vect2))
(define (vect2-add v1 v2)
  (vect2 (+ (vect2-x v1) (vect2-x v2))
         (+ (vect2-y v1) (vect2-y v2))))

; sub
(: vect2-sub (-> vect2 vect2 vect2))
(define (vect2-sub v1 v2)
  (vect2 (- (vect2-x v1) (vect2-x v2))
         (- (vect2-y v1) (vect2-y v2))))
