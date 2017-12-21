#lang racket

(define (process-line1 line)
  (let* ([seq (map string->number line)]
         [max-num (apply max seq)]
         [min-num (apply min seq)])
    (- max-num min-num)))

(define (process-line2 line)
  (define (apply-to-pair proc p)
    (proc (apply max p) (apply min p)))
  (let* ([seq (map string->number line)]
         [divisibles (filter (λ (p) (= (apply-to-pair modulo p) 0))
                         (pairs seq))]
         [divided (apply-to-pair / (flatten divisibles))])
    (flatten divided)))
    
(define (pairs lst)
  (cond [(eq? empty lst) empty]
        [(eq? empty (rest lst)) empty]
        [else (append (map (curry list (first lst))
                           (rest lst))
                      (pairs (rest lst)))]))

(define (main)
  (let* ([input (file->lines "advent2_input.txt")]
         [values (map string-split input)]
         [diff (map process-line1 values)]
         [result1 (apply + diff)]
         [divides (append-map process-line2 values)]
         [result2 (apply + divides)])
    (displayln "First half:")
    (displayln result1)
    (printf "Second half:\n~a" result2)))

(main)