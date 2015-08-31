#lang racket
(require racket/draw pict)
(require "../interval.rkt")

(define rule%
  (class object%
    (super-new)
    (field [width 600]
           [height 5]
           [color "gray"]
           [ticks 10]
           [base 10]
           [span (interval 0 1)]
           [children '()])

    (define/public (add iv #:color [color "red"])
      (set! children (cons (cons color iv) children)))

    (define (scale-x dx x)
      (let* ([x0 (interval-inf span)]
             [x1 (interval-sup span)]
             [a (/ width (interval-width span))])
        (+ dx (* a (- x x0)))))

    (define (interval-scale x k iv)
      (let ([ix (number->interval x)]
            [ik (number->interval k)])
        (interval+ ix (interval* ik iv))))

    (define/public (zoom-in k #:center [c (interval-mid span)])
      (let ([r (/ (interval-width span) 2 k)])
        (set! span (interval-scale c r interval-unit))
        (set! base (* k base))))

    (define/public (zoom-out k #:center [c (interval-mid span)])
      (let ([r (* (interval-width span) k)])
        (set! span (interval-scale c r interval-unit))
        (when (zero? (remainder base k))
          (set! base (/ base k)))))


    (define/public (adjust k)
      (set! base (quotient k (interval-width span))))
      
    (define (draw-rule dc dx y-rule y-text)
      (when ticks
        (send dc set-font (make-object font% ticks 'roman)))
      (send dc set-pen (new pen% [color "gray"]))
      (define n0 (* base (interval-inf span)))
      (define n1 (* base (interval-sup span)))
      (for ([k (range n0 (add1 n1))])
        (let ([x (scale-x dx (/ k base))]
              [txt (if (zero? k) "0" (format "~a/~a" k base))])
          (send dc draw-line x (- y-rule height) x (+ y-rule height))
          (define-values (tw th dp a) (send dc get-text-extent txt))
          (when ticks (send dc draw-text txt (- x (/ tw 2)) y-text))))
      (draw-scaled-line dc dx (interval-inf span) (interval-sup span) y-rule))

    (define (draw-scaled-line dc dx t0 t1 y)
      (let ([x0 (scale-x dx t0)]
            [x1 (scale-x dx t1)])
      (send dc draw-line x0 y x1 y)))
    
    (define (draw-interval dc dx iv k
                       #:color [color "red"]
                       #:bounds [text? #f]
                       #:pen-width [w 2])
      (let* ([dh (/ height 1)]
             [y (* (add1 k) dh)]
             [inf (interval-inf iv)]
             [sup (interval-sup iv)])
        (define old-pen (send dc get-pen))
        (send dc set-pen
              (new pen% [width w] [color color] [cap 'butt]))
        (draw-scaled-line dc dx inf sup y)
        (send dc set-pen old-pen)
        (when text?
          (send dc draw-text (number->string inf) (scale-x dx inf) (+ y dh))
          (send dc draw-text (number->string sup) (scale-x dx sup) (+ y dh)))))

    (define/public (draw dc dx dy)
      (let ([y (+ dy height)]
            [ht (* 2 height)]
            [k0 (* base (interval-inf span))]
            [k1 (* base (interval-sup span))])
        (define old-pen (send dc get-pen))
        (define old-font (send dc get-font))
        (draw-rule dc dx y (+ y ht))
        (for ([ch children] [k (in-range (length children))])
          (draw-interval dc dx (cdr ch) k #:color (car ch)))
        (send dc set-pen old-pen)
        (send dc set-font old-font)))))




;; Example

(define r (new rule%))
(send r add (interval (/ 1 2) (/ 2 3)))
(send r add (interval (/ 1 3) (/ 1 2)) #:color "blue")
(send r add (interval (/ 1 5) (/ 4 5)))
(dc (λ (dc dx dy) (send r draw dc dx dy)) 620 70)