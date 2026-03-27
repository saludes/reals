#lang racket
(require racket/generator)
;; See https://www.wikihow.com/Calculate-a-Square-Root-by-Hand
;; Method 2

(define (pure digits)
  (let ([ch0 (char->integer #\0)])
    (generator ()
               (for ([d digits])
                 (yield (- (char->integer d) ch0)))
               (for ([_ (in-naturals)]) (yield 0)))))

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

(define (10sqrt N)
  (generator ()
             (let loop ([p 0] [rest 0])
               (let*-values ([(n) (+ (* 100 rest) (* 10 (N)) (N))]
                             [(d rest1) (sqrt-one p n)])
               ; (displayln (list n d rest1))
               (yield d)
               (loop (+ (* 10 p) d) rest1)))))



          