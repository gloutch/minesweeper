#lang racket
(require "lib/vect2.rkt")
(require "lib/convex.rkt")
(require "minesweeper-game.rkt")
(require "ai.rkt")
(require graphics/graphics)

; UTILITARY
(struct Level (name path nb-bomb))
(define choices
  (list (list "a" (Level "Truncated Icosidodecahedron" "model/trunc_ico.obj" 10)) ; 62 faces
        (list "b" (Level "Snub Dodecahedron" "model/snub_dodeca.obj" 12)) ; 92
        (list "c" (Level "Deltoidal" "model/deltoidal.obj" 15))    ; 60
        (list "d" (Level "Icosphere" "model/icosphere.obj" 18))    ; 80
        (list "e" (Level "Icosphere+" "model/icosphere+.obj" 50))  ; 320
        (list "f" (Level "Icosahedron" "model/icosahedron.obj" 2)) ; 12
        (list "g" (Level "Snub Cube" "model/snub_cube.obj" 6))     ; 38
        (list "h" (Level "Sphere" "model/sphere.obj" 70))          ; 448
        (list "i" (Level "Pyramid" "model/pyramid.obj" 32))        ; 256
        (list "j" (Level "Cube" "model/cube.obj" 30))))            ; 384
        
(define (vect2->posn v) (make-posn (vect2-x v) (vect2-y v)))
(define (posn->vect2 p) (vect2     (posn-x  p) (posn-y  p)))


; GRAPHICS
(open-graphics)

(define window-size (make-posn 600 600))
(define window-center
  (make-posn (/ (posn-x window-size) 2) (/ (posn-y window-size) 2)))
(define info-pos
  (make-posn 20 (- (posn-y window-size) 20)))


(define nb-color 6)
(define c-min (make-rgb 0.8 0.8 1))
(define c-max (make-rgb 0 0 0.8))


(define (mix f1 f2 a)
  (+ (* a f1) (* (- 1 a) f2)))

(define (color nb)
  (let* ([t (/ nb nb-color)]
         [a (if (< t 1) t 1)])
    (make-rgb (mix (rgb-red   c-max) (rgb-red   c-min) a)
              (mix (rgb-green c-max) (rgb-green c-min) a)
              (mix (rgb-blue  c-max) (rgb-blue  c-min) a))))


(define (display-choice l)
  (letrec ([draw-one
            (lambda (p l)
              (if (null? l)
                  ((draw-string window) (make-posn 50 20) "Choose one solid")
                  (begin
                    ((draw-string window) p (string-append (caar l)
                                                           "  -  "
                                                           (Level-name (cadar l))))
                    (draw-one (make-posn (posn-x p)
                                         (+ (posn-y p) 20))
                              (cdr l)))))])
    (draw-one (make-posn 15 45) l)))


(define (draw-face id)
  (let ([coords (map vect2->posn (convex-project-face conv id))])
    (cond
      [(ms-is-found? game id)
       (if (ms-have-bomb? game id)
           ((draw-solid-polygon canvas) coords window-center "red")
           ((draw-solid-polygon canvas) coords window-center
                                        (color (ms-num-neighbours-bombs game id))))]
      [(ms-have-flag? game id)
       ((draw-solid-polygon canvas) coords window-center "orange")]
      
      [else
       ((draw-solid-polygon canvas) coords window-center "gray")])
    ((draw-polygon canvas) coords window-center "white")))


(define (draw-model m)
  ((clear-viewport canvas)) ; draw on canvas
  (map draw-face (convex-visible-faces m))
  (state-of-game)
  (copy-viewport canvas window)); flush on window


(define (draw-info str)
  (begin (draw-model conv)
         ((draw-string window) info-pos str)))


(define (quit)
  (begin (close-viewport canvas)
              (close-viewport window)
              (close-graphics)))



; EVENT HANDLER
(define rot 0.1)

(define (event-handler key-code useless)
  (let ([key (key-value key-code)])
    (begin
      (cond
        ; (rotation) [z - q - s - d]
        [(or (eq? key #\z) (eq? key 'up))    (begin
                                               (convex-rotate conv (- rot) 0)
                                               (draw-model conv))]
        [(or (eq? key #\s) (eq? key 'down))  (begin
                                               (convex-rotate conv rot 0)
                                               (draw-model conv))]
        [(or (eq? key #\q) (eq? key 'left))  (begin
                                               (convex-rotate conv 0 rot)
                                               (draw-model conv))]
        [(or (eq? key #\d) (eq? key 'right)) (begin
                                               (convex-rotate conv 0 (- rot))
                                               (draw-model conv))]
        ; (reveal - query) [e]
        [(and (eq? key #\e) (query-mouse-posn window))
         (reveal-query (convex-hit-face conv
                                        (vect2-sub (posn->vect2 (query-mouse-posn window))
                                                   (posn->vect2 window-center))))]
        ; (flag) [f]
        [(and (eq? key #\f) (query-mouse-posn window))
         (flag (convex-hit-face conv
                                (vect2-sub (posn->vect2 (query-mouse-posn window))
                                           (posn->vect2 window-center))))]
        ; (flag-help) [a]
        [(eq? key #\a) (flag-help)]
        ; (reveal-help) [tab]
        [(eq? key #\tab) (reveal-help)]
        ; (restart) [r]
        [(eq? key #\r) (restart)]
        ; (quit) [escape - x]
        [(or (eq? key 'escape) (eq? key #\x)) (quit)]))))




; GAME FUNCTION
(define (choose-game key)
  (cond
    [(eq? key #\a) (cadr (assoc "a" choices))]
    [(eq? key #\b) (cadr (assoc "b" choices))]
    [(eq? key #\c) (cadr (assoc "c" choices))]
    [(eq? key #\d) (cadr (assoc "d" choices))]
    [(eq? key #\e) (cadr (assoc "e" choices))]
    [(eq? key #\f) (cadr (assoc "f" choices))]
    [(eq? key #\g) (cadr (assoc "g" choices))]
    [(eq? key #\h) (cadr (assoc "h" choices))]
    [(eq? key #\i) (cadr (assoc "i" choices))]
    [(eq? key #\j) (cadr (assoc "j" choices))]
    [else (cadr (assoc "a" choices))]))


(define (restart)
  (begin (set! game (ms-graph-minesweeper graph nb-bombs))
         (draw-info "restart")))

(define (flag-help)
  (begin (set! game (apply-first-flag-rule game))
         (draw-info "flag-help")))

(define (reveal-help)
  (begin (set! game (apply-reveal-rule game))
         (draw-info "reveal-help")))

(define (reveal-query id)
  (cond [(eq? id -1) (draw-info "not a face")]
        
        [(ms-is-found? game id)
         (let ([nb (ms-num-neighbours-bombs game id)])
           (if (< nb 2)
               (draw-info (format "~a mine around"  (number->string nb)))
               (draw-info (format "~a mines around" (number->string nb)))))]
        
        [else (begin (set! game (ms-discover game id))
                     (draw-info "reveal"))]))

(define (flag id)
  (cond [(eq? id -1) (draw-info "not a face")]
        
        [(ms-is-found? game id) (draw-info "no flag here")]
        
        [(ms-have-flag? game id)
         (begin (set! game (ms-remove-flag game id))
                (draw-info "undo flag"))]
        [else
         (begin (set! game (ms-put-flag game id))
                (draw-info "flag"))]))

(define (state-of-game)
  (let ([state (ms-get-state game)])
    (when (not (eq? state 0))
      ((draw-string canvas) (make-posn (posn-x info-pos)
                                       (- (posn-y info-pos) 20))
                            (if (eq? state 1)
                                "Winner"
                                "Loser")))))



; GAME
(define canvas (open-pixmap   "offscreen"   window-size))
(define window (open-viewport "Minesweeper" window-size))

(display-choice choices)
(define curr-level (choose-game (key-value (get-key-press window))))


(define path (Level-path curr-level))
(define conv (convex-load path))
(convex-scale conv 250)

(define nb-bombs (Level-nb-bomb curr-level))
(define graph (convex->graph conv))
(define game (ms-graph-minesweeper graph nb-bombs))

(draw-info (Level-name curr-level))
((set-on-key-event window) event-handler)
