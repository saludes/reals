#lang racket
;; (require "interval.rkt")
(require racket/generator)
(struct interval2 (mantissa exp))

(define (find-valid-interval p)
  (let loop ([B 1] [e 0])
    (if (xor (p (- B)) (p B))
        (values e (cons (- B) B))
        (loop (* 2 B) (add1 e)))))

(define (bisect p)
  (define-values (exp I) (find-valid-interval p))
  (cons
   exp
   (generator ()
             (let loop ([a (car I)] [b (cdr I)])
               (define m (/ (+ a b) 2))
               (cond
                 [(xor (p a) (p m)) (yield #f) (loop a m)]
                 [(xor (p m) (p b)) (yield #t) (loop m b)]
                 [else (error "fail at [~a, ~a]" a b)])))))

(define (bits->interval exp bits)
  (define (mid a b) (/ (+ a b) 2))
  (define I0
    (let expand ([B 1] [n exp])
      (if (= n 0) (cons (- B) B) (expand (* 2 B) (sub1 n)))))
  (generator ()
             (let loop ([a (car I0)] [b (cdr I0)])
               (yield (cons a b))
               (if (bits)
                   (loop (mid a b) b)
                   (loop a (mid a b))))))
                 
                 
(define (imid I)
   (/ (+ (car I) (cdr I)) 2))
   


;; Decimal streams

(struct stream (radix exp digits))

(define (decimate p)
  (define (find10 I)
    (define dx (/ (- (cdr I) (car I)) 10))
    (define (scale t) (+ (car I) (* t dx)))
    (define j
      (for/last ([n (in-range 10)])
      #:final (xor (p (scale n)) (p (scale (add1 n))))
        n))
    (values j (cons (scale j) (scale (add1 j)))))
  (define-values (e I0)
    (let loop ([A 5] [e 0])
            (if (xor (p (- A)) (p A)) (values e (cons (- A) A)) (loop (* 10 A) (add1 e)))))
  (stream
   10
   e
   (generator ()
             (let loop ([I I0])
               (define-values (j Im) (find10 I))
               (yield j)
               (loop Im)))))


(define (stream-approximate st N)
  (define B (stream-radix st))
  (define I0
         (let expand ([I (cons (- (/ B 2)) (/ B 2))] [n (stream-exp st)])
           (if (= n 0) I (expand I (sub1 n)))))
  (define digits (stream-digits st))
  (let loop ([I I0] [k N])
    (let* ([a (car I)]
          [b (cdr I)]
          [dx (/ (- b a) B)]
          [scale (λ (t) (+ a (* t dx)))]
          [d (digits)])
      (if (= k 0)
          I
          (loop (cons (scale d) (scale (add1 d))) (sub1 k))))))
      
    
    



;; Examples

(define (sqr2 x) (and (> x 0) (> (* x x) 2)))
(define bsqrt2 (bisect sqr2))
(define dsqrt2 (decimate sqr2))


               

  