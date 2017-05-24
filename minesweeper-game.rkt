#lang typed/racket

(require "lib/integer-set.rkt")
(require "lib/graph.rkt")
(require "mines.rkt")
(require "game.rkt")

(provide Minesweeper
         Minesweeper-graph
         Minesweeper-bombs-map
         Minesweeper-flags-map
         Minesweeper-found-map
         Minesweeper-state
         ms-graph-minesweeper
         ms-is-found?
         ms-discover
         ms-put-flag
         ms-remove-flag
         ms-have-flag?
         ms-have-bomb?
         ms-neighbours
         ms-num-neighbours-bombs
         ms-num-vertex
         ms-num-not-found-neighbours
         ms-num-neighbours-flag
         ms-get-state
         ms-get-flags-map
         ms-get-found-map)

(struct: Minesweeper ([graph : Graph]
                      [bombs-map : Integer-set]
                      [flags-map : Integer-set]
                      [found-map : Integer-set]
                      [state : Integer]))



(: complete-aux? (Minesweeper -> Boolean))
(define (complete-aux? game)
  (= (+ (integer-set-size (Minesweeper-bombs-map game))
        (integer-set-size (Minesweeper-found-map game)))
     (graph-size (Minesweeper-graph game))))


(: new-game-state (Minesweeper Integer -> Integer))
(define (new-game-state game index)
  (cond ((not (zero? (Minesweeper-state game))) (Minesweeper-state game))
        ((have-bomb? (Minesweeper-bombs-map game) index) -1)
        ((complete-aux? game) 1)
        (else 0)))

;; Return the number of neighbours of index that math the predicat  (predicat game index)
(: num-neighbours-match (-> Minesweeper Integer (-> Minesweeper Integer Boolean) Integer))
(define (num-neighbours-match game index predicat)
  (let ((neighbours (ms-neighbours game index)))
    (: aux-predicat (-> Integer Boolean))
    (define (aux-predicat index)
      (predicat game index))
    (length (filter aux-predicat neighbours))))
  


;;  Provided functions


;; Create an minesweeper game with num-bombs bombs
(: ms-graph-minesweeper (-> Graph Integer Minesweeper))
(define (ms-graph-minesweeper graph num-bombs)
  (Minesweeper graph
               (fill-bombs (graph-size graph) num-bombs)
               (empty-integer-set)
               (empty-integer-set)
               0))


;;  Return wether index place has been found
(: ms-is-found? (-> Minesweeper Integer Boolean))
(define (ms-is-found? game index)
  (is-in-set? (Minesweeper-found-map game) index))

;; Discover recursively the index place
(: ms-discover (-> Minesweeper Integer Minesweeper))
(define (ms-discover game index)
  (let ((tmp-game
         (Minesweeper (Minesweeper-graph game)
                      (Minesweeper-bombs-map game)
                      (Minesweeper-flags-map game)
                      (reveal (Minesweeper-graph game)
                              index
                              (Minesweeper-bombs-map game)
                              (Minesweeper-found-map game))
                      (Minesweeper-state game))))
    (Minesweeper (Minesweeper-graph tmp-game)
                (Minesweeper-bombs-map tmp-game)
                (Minesweeper-flags-map tmp-game)
                (Minesweeper-found-map tmp-game)
                (new-game-state tmp-game index))))



;; Return wether the index place have a flags
(: ms-have-flag? (-> Minesweeper Integer Boolean))
(define (ms-have-flag? game index)
  (is-in-set? (Minesweeper-flags-map game) index))

;; Put a flag on the index place
(: ms-put-flag (-> Minesweeper Integer Minesweeper))
(define (ms-put-flag game index)
  (Minesweeper (Minesweeper-graph game)
               (Minesweeper-bombs-map game)
               (add-in-set (Minesweeper-flags-map game) index)
               (Minesweeper-found-map game)
               (Minesweeper-state game)))

;; Remove flag ont the index place
(: ms-remove-flag (-> Minesweeper Integer Minesweeper))
(define (ms-remove-flag game index)
  (Minesweeper (Minesweeper-graph game)
               (Minesweeper-bombs-map game)
               (integer-set-remove (Minesweeper-flags-map game) index)
               (Minesweeper-found-map game)
               (Minesweeper-state game)))
  
  
;; Return wether the index place have a bomb
(: ms-have-bomb? (-> Minesweeper Integer Boolean))
(define (ms-have-bomb? game index)
  (have-bomb? (Minesweeper-bombs-map game) index))

;; Return the number of bombs next to index
( : ms-num-neighbours-bombs (-> Minesweeper Integer Integer))
(define (ms-num-neighbours-bombs game index)
  (num-neighbours-match game index ms-have-bomb?))

;; Return the number of vertex in the game graph
( : ms-num-vertex (-> Minesweeper Integer))
(define (ms-num-vertex game)
  (graph-size (Minesweeper-graph game)))

;; Return index's neighbours
( : ms-neighbours (-> Minesweeper Integer (Listof Integer)))
(define (ms-neighbours game index)
  (graph-neighbours (Minesweeper-graph game) index))
  
;; Return the number of unfound neighbours
(: ms-num-not-found-neighbours (-> Minesweeper Integer Integer))
(define (ms-num-not-found-neighbours game index)
  (: is-not-found? (-> Minesweeper Integer Boolean))
  (define (is-not-found? game index)
    (not (ms-is-found? game index)))
  (num-neighbours-match game index is-not-found?))
    

;; Return the number of neighbours flag
(: ms-num-neighbours-flag (-> Minesweeper Integer Integer))
(define (ms-num-neighbours-flag game index)
  (num-neighbours-match game index ms-have-flag?))

;; Return wether the game is completed
(: ms-get-state (-> Minesweeper Integer))
(define (ms-get-state game)
  (Minesweeper-state game))

;; get flags map
(: ms-get-flags-map (-> Minesweeper Integer-set))
(define ms-get-flags-map
   Minesweeper-flags-map)

;; get flags map
(: ms-get-flags-map (-> Minesweeper Integer-set))
(define ms-get-found-map
   Minesweeper-found-map)