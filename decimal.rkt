#lang racket
(require "bisection.rkt" racket/generator)
(provide number->10 10/ 10sqrt)


(define (interval-decimate I)
  (define dx (/ (interval-size I) 10))
  (define (scale t) (+ (car I) (* t dx)))
  (for/list ([i (in-range 10)])
    (cons (scale i) (scale (add1 i)))))


(define (expand-by-until r I p)
  (define (expand-one I)
    (cons (* r (car I)) (* r (cdr I))))
  (let loop ([I I] [k 0])
    (if (p I)
        (values I k)
        (loop (expand-one I) (add1 k)))))
  

(define (log10 x)
  ; a non-real definition
  (define (find-exponent x)
    (define-values (_ n)
      (expand-by-until 10 (cons 0 1) (λ (I) (interval-contains? I (cons 0 x)))))
    n)
  (cond
    [(<= x 0) (error "must be positive: ~a" x)]
    [(< x 1) (- (log10 (/ x)))]
    [else (find-exponent x)]))

(define (10/ numerators denominator)
  ; Dec -> Int -> Dec
  (generator ()
             (yield (numerators))
             (let loop ([n (numerators)])
               (define-values (q r) (quotient/remainder n denominator))
               (yield q)
               (loop (+ (* 10 r) (numerators))))))
  

(define (string-integer->10 str)
  (define ch0 (char->integer #\0))
  (define B (expt 10 (string-length str)))
  (generator ()
             (yield (cons 0 B))
             (for ([ch (string->list str)])
               (yield (- (char->integer ch) ch0)))
             (let loop () (yield 0) (loop))))

(define (number->10 x)
  (cond
    [(string? x) (string-integer->10 x)]
    [(integer? x) (string-integer->10 (number->string x))]
    [else (10/
           (number->10 (numerator x))
           (denominator x))]))


(define (10->intervals decimals #:verbose [verbose? #f])
  (define I0 (decimals))
  (generator ()
             (let loop ([I I0])
               (yield I)
               (define d (decimals))
               (when verbose? (display d))
               (loop
                (list-ref (interval-decimate I) d)))))




(define (find-last f n)
  (for/last ([d (in-range 10)]
             #:when (<= (f d) n))
    d))


;; Square root as in 
;; https://www.wikihow.com/Calculate-a-Square-Root-by-Hand
;; Method 2

(define (sqrt-one p n)
  (let* ([p20 (* 20 p)]
         [f (λ (d) (* (+ p20 d) d))]
         [d (find-last f n)])
  (values d (- n (f d)))))
          
(define (10sqrt ns)
  (define I0 (ns)) ; assume positive
  (define exp (log10 (cdr I0)))
  (unless (and
           (zero? (car I0))
           (zero? (remainder exp 2)))
    (error "Must start with 0..10^(2n), got: ~a" I0))
  (define J0 (cons 0 (expt 10 (quotient exp 2))))
  (generator ()
             (yield J0)
             (let loop ([p 0] [rest 0])
               (let* ([d1 (ns)]
                      [d2 (ns)]
                      [n (+ (* 100 rest) (* 10 d1) d2)])
                 (define-values (d rest1) (sqrt-one p n))
                 (yield d)
                 (loop (+ (* 10 p) d) rest1)))))





  
  


