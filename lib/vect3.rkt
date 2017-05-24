#lang typed/racket
(provide (all-defined-out))


(struct: vect3 ([x : Real] [y : Real] [z : Real]))


; list->vect
(: list->vect3 (-> (List Real Real Real) vect3))
(define (list->vect3 l)
  (vect3 (car l) (cadr l) (caddr l)))

; print
(: vect3-print (-> vect3 Void))
(define (vect3-print v)
  (printf "(vect3 ~a ~a ~a)\n" (vect3-x v) (vect3-y v) (vect3-z v)))

; lenght
(: vect3-length (-> vect3 Real))
(define (vect3-length v)
  (sqrt (+ (sqr (vect3-x v))
           (sqr (vect3-y v))
           (sqr (vect3-z v)))))

; add
(: vect3-add (-> vect3 vect3 vect3))
(define (vect3-add v1 v2)
  (vect3 (+ (vect3-x v1) (vect3-x v2))
         (+ (vect3-y v1) (vect3-y v2))
         (+ (vect3-z v1) (vect3-z v2))))

; sub
(: vect3-sub (-> vect3 vect3 vect3))
(define (vect3-sub v1 v2)
  (vect3 (- (vect3-x v1) (vect3-x v2))
         (- (vect3-y v1) (vect3-y v2))
         (- (vect3-z v1) (vect3-z v2))))

; scale
(: vect3-scale (-> vect3 Real vect3))
(define (vect3-scale v a)
  (vect3 (* (vect3-x v) a)
         (* (vect3-y v) a)
         (* (vect3-z v) a)))

; dot product
(: vect3-dot (-> vect3 vect3 Real))
(define (vect3-dot v1 v2)
  (+ (* (vect3-x v1) (vect3-x v2))
     (* (vect3-y v1) (vect3-y v2))
     (* (vect3-z v1) (vect3-z v2))))

; cross product
(: vect3-cross (-> vect3 vect3 vect3))
(define (vect3-cross v1 v2)
  (vect3 (- (* (vect3-y v1) (vect3-z v2)) (* (vect3-z v1) (vect3-y v2)))
         (- (* (vect3-z v1) (vect3-x v2)) (* (vect3-x v1) (vect3-z v2)))
         (- (* (vect3-x v1) (vect3-y v2)) (* (vect3-y v1) (vect3-x v2)))))
