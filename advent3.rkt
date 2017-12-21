#lang racket

(require racket/match)
(require data/collection)


(define (double-stream s)
  (if (stream-empty? s)
      empty-stream
      (let ([first (stream-first s)])
        (stream* first first (double-stream (stream-rest s))))))


;(define (natural-numbers-stream)
;  (stream-cons 1 (stream-map (λ (n) (+ n 1)) natural-numbers)))


(define/match (next-direction dir)
  [('right) 'up]
  [('up)    'left]
  [('left)  'down]
  [('down)  'right]
  [(other)  (error 'unexpected-input)])


(define (in-range x low high)
  (and (<= low x) (<= x high)))


(define (manhattan-distance x y)
  (+ (abs x) (abs y)))


; part 1
(define (finder target)
  (define (new-pos x y dir distance-to-go)
    (match dir
      ['right (values (+ x distance-to-go) y)]
      ['left  (values (- x distance-to-go) y)]
      ['up    (values x (+ y distance-to-go))]
      ['down  (values x (- y distance-to-go))]))
  
  (define (walker target pos x y side-sizes direction)
    (define side-size (first side-sizes))
    (if (in-range target pos (+ pos side-size))
        (let-values ([(new-x new-y) (new-pos x y direction (- target pos))])
          (manhattan-distance new-x new-y))
        (let-values ([(new-x new-y) (new-pos x y direction side-size)])
          (walker target (+ pos side-size) new-x new-y (rest side-sizes) (next-direction direction)))))

  (walker target 1 0 0 (double-stream (in-naturals 1)) 'right))


; part 2
(define (tracker target)
  (define (check-trail trail x y)
    (hash-ref trail `(,x ,y) 0))
  
  (define (update-trail trail x y value)
    (hash-set trail `(,x ,y) value))

  (define (sum-neighbours trail x y)
      (for*/sum ([i '(-1 0 1)]
                 [j '(-1 0 1)])
        (check-trail trail (+ x i) (+ y j))));)
  
  (define (step x y direction)
    (match direction
      ['right (values (+ x 1) y)]
      ['left  (values (- x 1) y)]
      ['up    (values x (+ y 1))]
      ['down  (values x (- y 1))]))

  (define (side-walker x y steps side-size direction trail)
    (cond [(> (check-trail trail x y) target)
           (check-trail trail x y)]
          [(> steps 0)
           (let*-values ([(new-x new-y) (step x y direction)]
                         [(value)       (sum-neighbours trail new-x new-y)]
                         [(new-trail)   (update-trail trail new-x new-y value)])
             (side-walker new-x new-y (- steps 1) side-size direction new-trail))]
          [(or (eq? direction 'up) (eq? direction 'down))
           (side-walker x y (+ side-size 1) (+ side-size 1) (next-direction direction) trail)]
          [else
           (side-walker x y side-size side-size (next-direction direction) trail)]))
  
  (side-walker 0 0 1 1 'right #hash(((0 0) . 1))))