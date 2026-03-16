#lang racket
(require "interval.rkt")
(require racket/generator)
(struct interval2 (mantissa exp))


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

(define (find-valid-interval p)
  (let loop ([B 1] [e 0])
    (if (xor (p (- B)) (p B))
        (values e (cons (- B) B))
        (loop (* 2 B) (add1 e)))))

(struct bitstream (exp bits))

(define (bisect2 p)
  (define-values (exp I) (find-valid-interval p))
  (bitstream
   exp
   (generator ()
             (let loop ([a (car I)] [b (cdr I)] )
               (define m (/ (+ a b) 2))
               (cond
                 [(xor (p a) (p m)) (yield #\0) (loop a m)]
                 [else (yield #\1) (loop m b)])))))

(define (bits->interval bs)
  (define (mid a b) (/ (+ a b) 2))
  (define I0
    (let expand ([B 1] [n (bitstream-exp bs)])
      (if (= n 0) (cons (- B) B) (expand (* 2 B) (sub1 n)))))
  (define bits (bitstream-bits bs))
  (generator ()
             (let loop ([a (car I0)] [b (cdr I0)])
               (yield (cons a b))
               (case (bits)
                 [(#\0) (loop a (mid a b))]
                 [(#\1) (loop (mid a b) b)]
                 [else (error "expecting bit")]))))                 
                 
               
  
                

(define (imid I)
   (/ (+ (car I) (cdr I)) 2))
   
    
(define (sqr2 x) (and (> x 0) (> (* x x) 2)))
(define bsqrt2 (bisect2 sqr2))
(define il
  (for/last  ([I (in-producer (bits->interval bsqrt2))] [j (in-naturals)])
    #:break (> j 200)
    I))

  