#lang racket
(require "bisection.rkt")
(require racket/math racket/generator)
; see https://mathscholar.org/2019/02/simple-proofs-archimedes-calculation-of-pi/
(define (qsqrt n)
  (unless (> n 0)
    (error "~a must be >0" n))
  (bisect (λ (x) (> (* x x) n)) (cons 0 (max n 1))))

(define π
  (generator ()
             (let loop ([A (* 2 (sqrt 3))] [B 3])
               (yield (cons B A))
               (let ([A1 (/ (* 2 A B) (+ A B))]) 
                 (loop  A1 (sqrt (* A1 B)))))))