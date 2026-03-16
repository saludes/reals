#lang racket
(require "interval.rkt")
(require racket/generator)
(provide bisect)
(define (bisect p iv)
  (let* ([a (interval-inf iv)]
         [b (interval-sup iv)]
         [c (interval-mid iv)])
    (cond
      [(equal? (p a) (p c)) (interval c b)]
      [(equal? (p c) (p b)) (interval a c)]
      [else (error "invalid interval: ~e" iv)])))


(define (iterate f i0) (generator ()
  (let loop ([i i0])
    (begin
      (yield i)
      (loop (f i))))))

   
    

  