#lang racket

(require "vect3.rkt")
(provide parse-obj)


; return list of vertices and faces
(define (obj->lists file vertices face)
  (let ([line (read-line file)])
    
    (if (eof-object? line)       
        (cons vertices face)
        
        (let ([words (string-split line)])
          (cond [(string=? (car words) "v") ; vertex
                 (obj->lists file
                             (cons (list->vect3 (map string->number (cdr words))) vertices)
                             face)]
                [(string=? (car words) "f") ; face
                 (obj->lists file
                             vertices
                             (cons (map (lambda (x) (sub1 (string->number x))) (cdr words)) face))]
                ; unknow token, don't read the line but continue on the file
                [else (obj->lists file vertices face)])))))


(define (parse-obj path)
  (begin (define file (open-input-file path))
         (define lists (obj->lists file '() '()))
         (close-input-port file)
         (cons (list->vector (reverse (car lists)))
               (list->vector (cdr lists))))) ; return pair of vector (vertex face)


; TEST
; (parse-obj "../model/cube.obj")