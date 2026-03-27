#lang racket

(require racket/generator)
(provide bisect-one bisect forever-do)
(provide interval-size interval-mid interval-hull)

(define (interval-mid I)
  (/ (+ (car I) (cdr I)) 2))

(define (interval-size I)
  (- (cdr I) (car I)))

(define (interval-hull . Is)
  (let ([cars (map car Is)]
        [cdrs (map cdr Is)])
  (cons
   (apply min cars)
   (apply max cdrs))))


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

(define (sq2 x) (and (> x 0) (> (* x x) 2)))
(define bsqrt2
  (forever-do
   (λ (I) (bisect-one  sq2 I))
   (cons -2 2)))   
         

  
  
    

  