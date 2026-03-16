#lang racket
(require "interval.rkt")
(require "bisection.rkt")
(require racket/generator)

(define (iterate f i0) (generator ()
  (let loop ([i i0])
    (begin
      (yield i)
      (loop (f i))))))

;; Example

(define (sqr<2 x) (< (sqr x) 2))


(define (iterate-until g e)
  (for/last ([i (in-producer g)])
    #:break (< (interval-width i) e)
    i))


(define rsqrt2
  (λ (e)
    (let
        ([g (iterate (curry bisect sqr<2) (interval 0 2))])
      (iterate-until g e))))

(define (r+ x y)
  (λ (e)
    (let ([e1 (/ e 2)]
          [e2 (/ e 2)])
      (interval+ (x e1) (y e2)))))


(define (rneg x)
  (λ (e)
    (interval-neg (x e))))

(define (r- x y)
  (r+ x (rneg y)))


(define (r* x y)
  (define x1 (x 1))
  (define y1 (y 1))
  (define (ab i)
    (max (abs (interval-inf i)) (abs (interval-sup i))))
  (λ (e)
    (let ([d (/ e (+ (ab x1) (ab y1) 1))])
        (interval* (x d) (y d)))))


; test it by checking that rsqrt2^2 = 2

(define (check-sqrt2 e)
   (let ([r2 ((r* rsqrt2 rsqrt2) e)]) ; must be 2
     (unless (interval-in 2 r2)
       (error "2 not in interval (=~a)" e))
     (unless (<= (interval-width r2) e)
       (error "interval is largest than ~a" e)))
  (print (rsqrt2 e)))
       
        
             
    

  