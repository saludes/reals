#lang racket
(require racket/generator)
(require test-engine/racket-tests)
(require "bisection.rkt")
(provide dsqrt2 dsquare-root dcube-root)
(provide digits-to)

(define (find-valid-interval p)
  (let loop ([B 1] [e 0])
    (if (xor (p (- B)) (p B))
        (values e (cons (- B) B))
        (loop (* 2 B) (add1 e)))))



;; Decimal streams

(struct stream (radix exp digits))
(define (interval-decimate I)
  (define dx (/ (interval-size I) 10))
  (define (scale t) (+ (car I) (* t dx)))
  (for/list ([i (in-range 10)])
    (cons (scale i) (scale (add1 i)))))


(define (decide p I) (xor (p (car I)) (p (cdr I))))

(define (decimate-one p I)
  (define result
    (for/last ([J (interval-decimate I)] [i (in-range 10)])
      #:final (decide p J)
      (cons i J)))
  (display (car result))
  (cdr result))


(define (decimate* p I0)
  (forever-do (curry decimate-one p) I0))

(define (expand p)
  (define (expand-one I)
    (cons (* 10 (car I)) (* 10 (cdr I))))
  (let loop ([I (cons -1 1)])
    (if (decide p I) I (loop (expand-one I)))))


  
  
(define (decimate p)
  (define I-signed (expand p))
  (define I0
    (parameterize ([current-output-port (open-output-nowhere)]) ; put in a macro
      (bisect-one p I-signed)))
  (decimate* p I0))

(define (digits-to g e)
  (define (mirror ch)
      (let ([ch0 (char->integer #\0)])
        (case ch
          [(#\.) #\.]
          [else ((compose1 integer->char (λ (c) (+ 9 (* 2 ch0) (- c))) char->integer) ch)])))
  (define mirror-string
    (compose1 list->string (curry map mirror) string->list))
  (define (steps x)
    (inexact->exact (round (log x 10))))
   (define digits
     (with-output-to-string
     (λ ()
       (define I0  (g))
       (display (if (zero? (car I0)) #\+ #\-))
       ;(define N  (steps size))
       (for ([I (in-producer g)]
             [j (in-naturals 1)])
         #:break (> j e)
         (when (= (interval-size I) 1) (display #\.))))))
  (case (string-ref digits 0)
    [(#\-) (string-append "-" (mirror-string (substring digits 1)))]
    [else  digits])) 
  
         ;(when (zero? (- N j)) (display #\.))))))
  
     
  


;; Examples

(define (dsquare-root n)
  (decimate (λ (x) (and (>= x 0) (> (* x x) n)))))
(define (dcube-root n)
  (decimate (λ (x) (> (* x x x) n))))
(define dsqrt2 (dsquare-root 2))


;; Test
(module+ test
  (require rackunit)
  (define-simple-check (check-small? I e)
    (<= (interval-size I) e))
  
  (define-simple-check (check-is-cut-for? I f)
    (<= (* (f (car I)) (f (cdr I))) 0))
  
  (define (sq x) (* x x))

  (define (approximate g steps)
    (for/last ([I (in-producer g)]
               [j (in-naturals)])
        #:break (> j steps)
      I))
  (define Is2 (approximate dsqrt2 100))
  (check-small? Is2 1e-80)
  (check-is-cut-for? Is2 (λ (x) (- (sq x) 2))))
