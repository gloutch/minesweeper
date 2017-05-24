#lang typed/racket

(require "../lib/tree.rkt")


(: ex1 (Tree Integer))
(define ex1 '((nil 8 nil) 0 (nil 7 (nil 1 nil))))

(: ex2 (Tree Float))
(define ex2 '((nil 8. nil) 0. (nil 7. (nil 1. nil))))

(: ex3 (Tree String))
(define ex3 '((nil "eight" nil) "zero" (nil "seven" (nil "one" nil))))


;; TODO: make some test, even if its works