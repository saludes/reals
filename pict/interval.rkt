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
           [nums (cons 0 10)]
           [children '()])
    (define/public (add iv #:color [color "red"])
      (set! children (cons (cons color iv) children)))
    (define/public (draw dc dx dy)
      (let ([sc (/ width base)]
            [y (+ dy height)]
            [ht (* 2 height)])
        (define old-pen (send dc get-pen))
        (define old-font (send dc get-font))
        (send dc set-pen (new pen% [color "gray"]))
    (when ticks
      (send dc set-font (make-object font% ticks 'roman)))
        (for ([k (range (car nums) (add1 (cdr nums)))])
          (let ([x (+ dx (* k sc))]
                [txt (if (eq? k 0) "0" (format "~a/~a" k base))])
            (send dc draw-line x (- y height) x (+ y height))
            (define-values (tw th dp a) (send dc get-text-extent txt))
            (when ticks (send dc draw-text txt (- x (/ tw 2)) (+ dy ht)))))
        (send dc draw-line dx y (+ dx width) y)
        (for ([ch children] [k (in-range (length children))])
          (draw-interval dc this (cdr ch) k #:color (car ch)))
        (send dc set-pen old-pen)
        (send dc set-font old-font)))))



(define (draw-interval dc rule iv k
                       #:color [color "red"]
                       #:width [w 2])
  (let* ([rule-width (get-field width rule)]
        [rule-height (get-field height rule)]
        [dh (/ rule-height 1)]
        [y (* (add1 k) dh)]
        [base (get-field base rule)]
        [sc (λ (t) (* t (/ rule-width base)))]
        [inum (* base (interval-inf iv))]
        [snum (* base (interval-sup iv))])
    (define old-pen (send dc get-pen))
    (send dc set-pen
          (new pen% [width w] [color color] [cap 'butt]))
    (send dc draw-line (sc inum) y (sc snum) y)
    (send dc set-pen old-pen)
    (send dc draw-text (number->string (interval-inf iv)) (sc inum) (+ y dh))
    (send dc draw-text (number->string (interval-sup iv)) (sc snum) (+ y dh))))



;; Example

(define r (new rule%))
(send r add (interval (/ 1 2) (/ 2 3)))
(send r add (interval (/ 1 3) (/ 1 2)) #:color "blue")
(send r add (interval (/ 1 5) (/ 4 5)) #:color "magenta")
(dc (λ (dc dx dy) (send r draw dc dx dy)) 620 70)