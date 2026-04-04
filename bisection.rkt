#lang racket

(require racket/generator)
(provide bisect-one bisect forever-do)
(provide interval-size interval-mid
         interval-cup interval-cap interval-contains?
         is-in? natural-map is-cut?
         format-interval)
         
(define (interval-mid I)
  (/ (+ (car I) (cdr I)) 2))

(define (interval-size I)
  (- (cdr I) (car I)))


(define (is-cut? I p)
  (xor (p (car I)) (p (cdr I))))

(define (is-in? x I)
  (<= (car I) x (cdr I)))

(define (interval-cap . Is)
  (let* ([cars (map car Is)]
         [cdrs (map cdr Is)]
         [m1 (apply max cars)]
         [m2 (apply min cdrs)])
    (if (<= m1 m2) (cons m1 m2) #f)))

(define (interval-contains? I J)
  (equal? (interval-cap I J) J))


(define (interval-cup . Is)
  (let ([cars (map car Is)]
         [cdrs (map cdr Is)])
    (cons (apply min cars) (apply max cdrs))))


(define (natural-map f I)
  (let ([f1 (f (car I))]
        [f2 (f (cdr I))])
    (cons (min f1 f2) (max f1 f2))))

(define (format-interval I)
  (define (pad-with str ch n)
    (let ([n-pad (- n (string-length str))])
    (string-append
     str
     (list->string (for/list ([k (in-range n-pad)]) ch)))))
  (let* ([eps (/ (interval-size I) 2)]
        [m (interval-mid I)]
        [n-decimals (- (inexact->exact (round (log eps 10))))]
        [m-prec 10]
        [m-str (~r m #:notation 'positional #:precision (max 10 n-decimals))])
    (format "~a±~a"
            m-str ; (pad-with m-str #\. n-decimals)
            (~r eps #:notation 'exponential #:precision 0))))



(define (pair->list p) (list (car p) (cdr p)))
  

(define (fcup f #:extra [vs0 '()] . Is)
  ; (a ... -> I b) -> [a] ...  -> I b
  (let* ([vs1 (apply cartesian-product (map pair->list Is))]
         [vs void])
    (fcup* f vs)))

(define (is-in*? p* . I*)
  (for/and ([I I*] [p p*]) (is-in? p I)))


(define (natural* f #:extra [vs0 '()] . Is)
  (let* ([f^ (compose1 pure^ f)]
        [vs1 (filter (λ(v) (apply is-in*? v Is)) vs0)]
        [vs (append vs1 (apply cartesian-product (map pair->list Is)))])
    (displayln vs)
  (fcup* f^ vs)))
  
  
  

(define (pure^ x) (cons x x))

(define (fcup* f  vs)
  (let ([Js (map (curry apply f) vs)])
    (apply interval-cup Js)))
        

(define (natural1 f I #:extra [xs '()])
  ; (a -> b) -> I a -> I b
  (let ([f^ (compose1 pure^ f)]
        [vs 
         (append
          (list (car I) (cdr I))
          (filter (curryr is-in? I) xs))])
    (fcup* f^ (map list vs)))) 
  
  


(define (natural f I)
  (fcup (λ (x) (let ([y (f x)]) (list y y))) I))


(define (bisect-one p iv)
  (let* ([a (car iv)]
         [b (cdr iv)]
         [m (interval-mid iv)])
    (cond
      [(xor (p a) (p m)) (display 0) (cons a m)]
      [(xor (p m) (p b)) (display 1) (cons m b)]
      [else (error "invalid interval: ~e" iv)])))


(define (forever-do f x0)
  (generator ()
             (let loop ([x x0])
               (yield x)
               (loop (f x)))))

(define (bisect p I0)
  (forever-do (curry bisect-one p) I0))

;; Example

(module+ test
  (require rackunit)
  (define-simple-check (check-small? I e)
    (<= (interval-size I) e))
  (define-simple-check (check-wraps? I f y)
    (check-true (is-cut? I (λ (x) (< (f x) y)))))

  (let* ([sq2  (λ (x) (and (> x 0) (> (* x x) 2)))]
         [bsqrt2
          (forever-do
           (λ (I) (bisect-one sq2 I))
           (cons -2 2))]
         [exp 10]
         [Ifinal
          (for/last ([j (in-range (+ 3 exp))])
            (bsqrt2))]) ; 1 for sign, 1 to (cons 0 1)
    (check-small? Ifinal (/ (expt 2 exp)))
    (check-wraps? Ifinal sqr 2))

  (check-true
   (interval-contains? (cons 0 2) (cons 1 2)))
  (check-false
   (interval-contains? (cons 0 2) (cons 1 3)))
)         

  
  
    

  