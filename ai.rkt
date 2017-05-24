#lang typed/racket

(require "minesweeper-game.rkt")
(require "lib/integer-set.rkt")
(require "lib/graph.rkt")

(provide apply-first-flag-rule apply-reveal-rule)

;;;; Tools function


;; x is in l ?
( : typed-member? (All (A) (-> A (Listof A) Boolean)))
(define (typed-member? x l)
  (if (null? l)
      #f
      (or
       (eq? x (car l))
       (typed-member? x (cdr l)))))


;; Forall x in l1, x is in l2 ?
( : contain? (All (A) (-> (Listof A) (Listof A) Boolean)))
(define (contain? l1 l2)
  (: aux (-> A Boolean Boolean))
  (define (aux x b)
    (and b (typed-member? x l2) b))
  ((inst foldl A Boolean Void Void)
   aux
   #t
   l1))

;; Return the complement of a in b (set theory)
( : complement (All (A) (-> (Listof A) (Listof A) (Listof A))))
(define (complement a b)
  (: predicat (-> A Boolean))
  (define (predicat x)
    (not (typed-member? x a)))
  (filter predicat b))

;; Return the unfounded neighboors list
( : unfound-neighbours (-> Minesweeper Integer (Listof Integer)))
(define (unfound-neighbours game index)
  (: predicat? (-> Integer Boolean))
  (define (predicat? index)
    (not (ms-is-found? game index)))
  (filter predicat? (ms-neighbours game index)))


;; Return the found map border
(: found-border (-> Minesweeper (Listof Integer)))
(define (found-border game)
  (: is-on-border? (-> Integer Boolean))
  (define (is-on-border? index)
    (and (ms-is-found? game index)
         (not
          (= 0
             (length (unfound-neighbours game index))))))
  (: aux (-> Integer (Listof Integer)))
  (define (aux index)
    (cond ((= index (graph-size (Minesweeper-graph game)))
           '())
          ((is-on-border? index)
           (cons index (aux (add1 index))))
          (else
           (aux (add1 index)))))
  (aux 0))

;; Flag a list of index
;; Precond : they are not found

( : flag-list (-> Minesweeper (Listof Integer) Minesweeper))
(define (flag-list game l)
  (: aux (-> Integer Minesweeper Minesweeper))
  (define (aux index game)
    (ms-put-flag game index))
  ((inst foldl Integer Minesweeper Void Void) aux game l))


;; Reveal a list of index
;; Precond : they are not found

( : reveal-list (-> Minesweeper (Listof Integer) Minesweeper))
(define (reveal-list game l)
  (: aux (-> Integer Minesweeper Minesweeper))
  (define (aux index game)
    (ms-discover game index))
  ((inst foldl Integer Minesweeper Void Void) aux game l))



;;
;;  Flag rules
;;


;; First flag rule (|neighboors| == |bomb|), Precond index discovered (safe rule)
(: first-flag-rule (-> Integer Minesweeper Minesweeper))
(define (first-flag-rule index game)
  (let ((v (unfound-neighbours game index))
        (nb (ms-num-neighbours-bombs game index)))
    (if (= nb (length v))
        (flag-list game v)
        game)))

;; Precond i1 != i2, i1 and id dicovered  (safe rule)
(: second-flag-rule (-> Minesweeper Integer Integer Minesweeper))
(define (second-flag-rule game i1 i2)
  (let ((v1 (unfound-neighbours game i1))
        (v2 (unfound-neighbours game i2)))
    (if (and
         (contain? v1 v2)
         (=
          (- (length v2)
             (length v1))
          (- (ms-num-neighbours-bombs game i2)
             (ms-num-neighbours-bombs game i1))))
        (flag-list game (complement v1 v2))
        game)))

;; Flags rules aplication 

( : apply-first-flag-rule (-> Minesweeper Minesweeper))
(define (apply-first-flag-rule game)
  (let ((border (found-border game)))
    ((inst foldl Integer Minesweeper Void Void)
     first-flag-rule game border)))



;; Reveal Rule
;; Precond index discovered


(struct: Safe-reveal-rule-param ([game : Minesweeper]
                                 [nb-reveal : Integer]
                                 [min-prob : Exact-Rational]
                                 [index-min-prob : Integer]))

(: reveal-rule (-> Integer Safe-reveal-rule-param Safe-reveal-rule-param))
(define (reveal-rule index param)

  (let ((game (Safe-reveal-rule-param-game param))
        (nb-reveal (Safe-reveal-rule-param-nb-reveal param))
        (min-prob (Safe-reveal-rule-param-min-prob param))
        (index-min-prob (Safe-reveal-rule-param-index-min-prob param)))
  
  (let ((v
         (unfound-neighbours  game index)))
      
      (: pred-flag? (-> Integer Boolean))
      (define (pred-flag? index)
        (ms-have-flag? game index))
      
      (let ((v-flag
             (filter pred-flag? v))
            (nb-bombs
             (ms-num-neighbours-bombs game index))
            (nb-neighbours
             (length v)))
        (let ((nb-flags
               (length v-flag)))
        
          (if (not (= nb-flags nb-neighbours))   ;; If there is a neighbour tot reveal
            
              (if (= nb-flags nb-bombs) ;; Safe reveal, do it

                 (Safe-reveal-rule-param (reveal-list game (complement v-flag v))
                                    (add1 nb-reveal)
                                    min-prob
                                    index-min-prob)
                 ;; else : Record proability of having bomb and AN index concerned by the probability if this is the min

                 (let ((proba
                        (/ (- nb-bombs nb-flags) (- nb-neighbours nb-flags))))
                   (if (< proba min-prob)  ;; less than the curent min
                       (Safe-reveal-rule-param game
                                               nb-reveal
                                               proba
                                               (car (complement v-flag v))) ;; choosing one index
                       param)))
            
            param))))))
  
(: random-reveal (-> Minesweeper Minesweeper))
(define (random-reveal game)
  (: ident (-> Integer Integer))
  (define (ident x) x)
  (: aux (-> Minesweeper (Listof Integer) Minesweeper))
  (define (aux game index-list)
    (if (null? index-list)
        game
        (if (not (or (ms-is-found? game (car index-list))
                     (ms-have-flag? game (car index-list))))
            (ms-discover game (car index-list))
            (aux game (cdr index-list)))))
  (aux game (shuffle
             ((inst build-list Integer)
              (graph-size (Minesweeper-graph game))
              ident))))

(: apply-reveal-rule (-> Minesweeper Minesweeper))
(define (apply-reveal-rule game)
  (let ((border (found-border game))
        (param (Safe-reveal-rule-param game 0 2 -1)))
    (let
        ((result ((inst foldl Integer Safe-reveal-rule-param Void Void)
                  reveal-rule param border)))

      (if (not (= 0 (Safe-reveal-rule-param-nb-reveal result)))  ;; if safe reveal has been done
          (begin
            (display "Safe deterministic reveal\n")
            (Safe-reveal-rule-param-game result))

          (begin
            (let ((index
                   (Safe-reveal-rule-param-index-min-prob result)))
              (if (not (= -1 index))  ;; there is unsafe reveal ( chosen )
                  (begin
                    (display "Random chosen reveal\n")
                    (display (Safe-reveal-rule-param-min-prob result))
                    (display "\n")
                    (ms-discover game index))
                  (begin
                    (display "Uniform Random reveal\n")
                    (random-reveal game)))))))))



