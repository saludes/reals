#lang racket
(require "interval.rkt")

(define (bisect p iv)
  (let* ([a (interval-inf iv)]
         [b (interval-sup iv)]
         [c (interval-mid iv)])
    (cond
      [(equal? (p a) (p c)) (interval c b)]
      [(equal? (p c) (p b)) (interval a c)]
      [else (error "invalid interval: ~e" iv)])))

;; Example

(define (sqr2 x) (< (sqr x) 2))
(define bs-sqr (curry bisect sqr2))
(define i0 (interval 1 2))

  