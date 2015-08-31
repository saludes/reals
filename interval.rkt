#lang racket

(provide
 (except-out (all-defined-out)
             interval-operate
             interval-print))
 
(define (interval-print iv port mode)
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (write-string "[" port)
    (recur (interval-inf iv) port)
    (write-string ", " port)
    (recur (interval-sup iv) port)
    (write-string "]" port)))
         

(struct interval (inf sup)
  #:guard (λ (inf sup type-name)
            (let ([error-rat (λ (r) (error type-name "not rational ~e" r))]
                  [exact-rational? (λ (r) (and (rational? r) (exact? r)))]) 
              (cond
                [(not (exact-rational? inf)) (error-rat inf)]
                [(not (exact-rational? sup)) (error-rat sup)]
                [(> inf sup) (error type-name "improper bounds ~e >  ~e" inf sup)]
                [else (values inf sup)])))              
  #:methods gen:custom-write
  [(define write-proc interval-print)])

(define (interval-width iv) (- (interval-sup iv) (interval-inf iv)))
(define (interval-mid iv) (/ (+ (interval-inf iv) (interval-sup iv)) 2))
(define (interval+ i . is)
  (let (
        [infs (map interval-inf (cons i is))]
        [sups (map interval-sup (cons i is))])
    (interval (apply + infs) (apply + sups))))
(define (interval-neg i)
    (interval (- (interval-sup i)) (- (interval-inf i))))

(define (number->interval x) (interval x x))

(define (interval* i j)
  (let* ([iv->list (λ (iv) (list (interval-inf iv) (interval-sup iv)))]
         [abs (for*/list ([a (iv->list i)] [b (iv->list j)]) (* a b))])
    (interval (apply min abs) (apply max abs))))

(define interval-unit (interval -1 1))

(define (interval-operate fi fs ivs)
  (let ([inf (apply fi (map interval-inf ivs))]
        [sup (apply fs (map interval-sup ivs))])
    (interval inf sup)))

(define (interval-hull i . is)
  (interval-operate min max (cons i is)))

(define (interval-cut i . is)
  (let ([ic (interval-operate max min (cons i is))])
    (if (interval-empty? ic) #f ic)))

(define (interval-empty? i)
  (> (interval-inf i) (interval-sup i)))

(define (interval/ i)
  (if (interval-in 0 i)
      (error "Unbound reciprocal of" i)
      (interval (/ (interval-sup i)) (/ (interval-inf i)))))

(define (interval-in x i)
  (and (<= (interval-inf i) x) (<= x (interval-sup i))))

(define (interval-expt i n)
  (cond ([zero? n] (number->interval 1))
        ([< n 0] (interval/ (interval-expt i (- n))))
        ([even? n] (let* ([inf (interval-inf i)]
                          [sup (interval-sup i)]
                          [is (min (expt inf n) (expt sup n))]
                          [es (max (expt inf n) (expt sup n))])
                     (if (interval-in 0 i)
                         (interval 0 es)
                         (interval is es))))
        (else (interval (expt (interval-inf i) n) (expt (interval-sup i) n)))))