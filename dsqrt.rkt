#lang racket
(require racket/generator)
(require "binary.rkt")
(provide 10sqrt number->10)

;; See https://www.wikihow.com/Calculate-a-Square-Root-by-Hand
;; Method 2


(define (log10* x)
  ((compose1 inexact->exact ceiling) (log x 10)))

(define (log10 x)
  ((compose1 inexact->exact round) (log x 10)))


(define (number->10 n #:exp [exp0 #f])
  (unless (> n 0)
    (error "must be positive" n))
  (define B (expt 10 (if exp0 exp0 (log10* n))))
  (define (cut x) (<= x n))
  (define I0 (cons 0 B))
  (generator ()
             (yield #f I0)
             (let loop ([I I0])
               (let* ([kJ (find-decimated-cut cut I)]
                      [k (first kJ)]
                      [J (second kJ)])
                 (yield  k J)
                 (loop J)))))
                 
                        

(define (find-decimated-cut f I)
  (for/last ([J (interval-decimate I)]
             [k (in-range 10)])
             #:final (xor (f (car J)) (f (cdr J)))
             (list k J)))
                                       
    


(define (find-last f n)
  (for/last ([d (in-range 10)]
             #:when (<= (f d) n))
    d))
    

(define (sqrt-one p n)
  (let* ([p20 (* 20 p)]
         [f (λ (d) (* (+ p20 d) d))]
         [d (find-last f n)])
  (values d (- n (f d)))))

(define (split-string-at str n)
  (values (substring str 0 n) (substring str n)))
          

(define (10sqrt ns)
  (define-values (_ I0) (ns))
  (define exp0 (log10 (cdr I0)))
  (unless (and
           (zero? (car I0))
           (zero? (remainder exp0 2)))
      (error "Only streams with initial 0..10^(2n) are allowed. Got " I0))
  (define J0 (cons 0 (expt 10 (quotient exp0 2))))
  (generator ()
             (yield 10 J0)
             (let loop ([p 0] [rest 0] [I J0])
               (let*-values ([(d1 _) (ns)]
                             [(d2 _) (ns)]
                             [(n) (+ (* 100 rest) (* 10 d1) d2)]
                             [(d rest1) (sqrt-one p n)]
                             [(I1) (list-ref (interval-decimate I) d)])
                 (yield d I1)
                 (loop (+ (* 10 p) d) rest1 I1)))))


; Testing 

(module+ test
  (require rackunit "bisection.rkt")
  (define-simple-check (check-small? I e)
    (<= (interval-size I) e))
  (define-simple-check (check-cuts? f n I)
    (is-cut? I (λ (x) (> (f x) n))))
  (define sqrt10 (10sqrt (number->10 10 #:exp 2)))
  (define-values (d0 I0) (sqrt10))
  (check-eq? d0 10 "not a decimal stream")
  (check-eq? (log10 (interval-size I0)) 1)
  (define If
    (for/last ([j (in-range 100)]) 
      (define-values  (d J) (sqrt10))
      (cond
        [(not d)]
        [(= (interval-size J) 1) (display d) (display ".")]
        [else (display d)]) ; comment out to supress output
      J))      
(check-cuts? sqr 10 If)
(check-small? If 1e-99))

       
    
          