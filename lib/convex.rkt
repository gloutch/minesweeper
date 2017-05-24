#lang typed/racket
(require "graph.rkt")
(require "vect3.rkt")
(require "vect2.rkt")

(define-type Vertices (Vectorof vect3))
(define-type Face     (Listof Integer))
(define-type Faces    (Vectorof Face))
(require/typed "obj_parser.rkt"
               [parse-obj (-> Path-String (Pairof Vertices Faces))])

(struct: Convex ([nbv : Integer]
                 [vertices : Vertices]
                 [nbf : Integer]
                 [faces    : Faces]))

(provide Convex
         convex-load
         convex-scale
         convex-rotate
         convex-visible-faces
         convex-project-face
         convex-hit-face
         convex->graph)

(: view-vect vect3) ; vect3 to look the model
(define view-vect (vect3 0. 0. 1.))


(: convex-load (-> Path-String Convex))
(define (convex-load path)
  (let ([vectors (parse-obj path)])
    (Convex (vector-length (car vectors))
            (car vectors) ; vertices
            (vector-length (cdr vectors))
            (cdr vectors)))) ; faces

(: convex-scale (-> Convex Real Vertices))
(define (convex-scale s a)
  (vector-map! (λ ([x : vect3]) (vect3-scale x a))
               (Convex-vertices s)))


(: convex-rotate (-> Convex Real Real Vertices))
(define (convex-rotate s rx ry)
  (let ([cx (cos rx)][sx (sin rx)]
        [cy (cos ry)][sy (sin ry)])
    (vector-map!
     (λ ([v : vect3])
        (let ([x (vect3-x v)]
              [y (vect3-y v)]
              [z (vect3-z v)])
          (vect3 (+ (* x cy) (* z sy))
                 (+ (* x sx sy) (* y cx) (* z (- sx) cy))
                 (+ (* x cx (- sy)) (* y sx) (* z cx cy)))))
     (Convex-vertices s))))


; project face id in 2d
(: convex-project-face (-> Convex Integer (Listof vect2)))
(define (convex-project-face s id)
  (map (λ ([i : Integer])
         (let ([v (vector-ref (Convex-vertices s) i)])
           (vect2 (vect3-x v) (vect3-y v))))
       (vector-ref (Convex-faces s) id)))


; list of index of visible face
(: convex-visible-faces (-> Convex (Listof Integer)))
(define (convex-visible-faces s)
  (filter (λ ([id : Integer]) (visible-face? s id))
          (range (Convex-nbf s))))

(: visible-face? (-> Convex Integer Boolean))
(define (visible-face? s id)
  (let ([face (vector-ref (Convex-faces s) id)] ; (Listof index of vertex)
        [curr (Convex-vertices s)])
    (let ([v0 (vector-ref curr (car face))]     ; vertex are planar
          [v1 (vector-ref curr (cadr face))]    ; just need 3 of them
          [v2 (vector-ref curr (caddr face))])  ; in the right order
      (positive? (vect3-dot view-vect
                            (vect3-cross (vect3-sub v2 v1)
                                         (vect3-sub v0 v1)))))))

; index of hit face or -1
(: convex-hit-face (-> Convex vect2 Integer))
(define (convex-hit-face s point)
  (hit-face s
            (range (Convex-nbf s))
            point))

(: hit-face (-> Convex Face vect2 Integer))
(define (hit-face s face point)
  (if (null? face)
      -1
      (if (visible-face? s (car face)) ; the face is visible
          (let ([face-coords (convex-project-face s (car face))])
            (if (inside-polygon? (car face-coords) face-coords point)
                (car face) ; and point is inside
                (hit-face s (cdr face) point)))
          (hit-face s (cdr face) point))))

; point is inside a convex polygon?
(: inside-polygon? (-> vect2 (Listof vect2) vect2 Boolean))
(define (inside-polygon? first-coord list-coord point)
  (if (null? (cdr list-coord))                       ; just one last coord
      (at-left? (car list-coord) first-coord point) ; ckeck edge (last -> first)
      (and (at-left? (car list-coord) (cadr list-coord) point)
           (inside-polygon? first-coord (cdr list-coord) point))))

; p is it at right of the edge (v1 -> v2)?
(: at-left? (-> vect2 vect2 vect2 Boolean))
(define (at-left? v1 v2 p)
  (positive?
   (+ (* (- (vect2-x v2) (vect2-x v1))
         (- (vect2-y p ) (vect2-y v1)))
      (* (- (vect2-y v1) (vect2-y v2))
         (- (vect2-x p ) (vect2-x v1))))))


; construct the graph of the model
; vertices are neigbor if the faces are adjacent
(: convex->graph (-> Convex Graph))
(define (convex->graph s)
  (cons (Convex-nbf s) (list-Vertex s)))

(: list-Vertex (-> Convex (Listof Vertex)))
(define (list-Vertex s)
  (map (lambda ([id : Integer])
         (Vertex id (list-neigh s id (vector-ref (Convex-faces s) id))))
       (range (Convex-nbf s))))

(: list-neigh (-> Convex Integer Face (Listof Integer)))
(define (list-neigh s id face)
  (filter (lambda ([i : Integer])
            (adjacent? face (vector-ref (Convex-faces s) i) 0))
          (remove id (range (Convex-nbf s)))))

; faces are adjacent if they have exactly two same vertices
(: adjacent? (-> Face Face Integer Boolean))
(define (adjacent? f1 f2 nb)
  (cond [(eq? nb 2) #t]
        [(null? f1) #f]
        [else (adjacent? (cdr f1) f2 (if (member (car f1) f2)
                                         (add1 nb)
                                         nb))]))
  

  