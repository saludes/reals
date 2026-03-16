#lang racket
; (require "interval.rkt")
(require racket/generator)
(provide bisect)

(define (mid I)
  (/ (+ (car I) (cdr I)) 2))

(define (bisect-one p iv)
  (let* ([a (car iv)]
         [b (cdr iv)]
         [m (mid iv)])
    (cond
      [(xor (p a) (p m)) (cons a m)]
      [(xor (p m) (p b)) (cons m b)]
      [else (error "invalid interval: ~e" iv)])))


(define (bisect p I0)
  (generator ()
             (let loop ([I I0])
               (yield I)
               (loop (bisect-one p I))))) 
    

  