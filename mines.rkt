#lang typed/racket

(require "lib/integer-set.rkt")
(provide fill-bombs have-bomb?)


(: fill-bombs (-> Integer Integer Integer-set))
(define (fill-bombs size num)
  (boolean-list->integer-set
   (shuffle
    (append (make-list num #t)
            (make-list (- size num) #f)))))

(: have-bomb? (-> Integer-set Integer Boolean))
(define (have-bomb? bombs-map index)
  (is-in-set? bombs-map index))

