#lang racket


(define (break-free lst updater)
  (define (cpu lst pos steps-done)
    (if (or (> 0 pos) (>= pos (length lst)))
        steps-done
        (let ([next-pos (+ pos (list-ref lst pos))]
              [new-lst (list-update lst pos updater)]
              [new-steps-done (+ steps-done 1)])
          (cpu new-lst next-pos new-steps-done))))
  (cpu lst 0 0))
    


(define (read-from-file file)
  (with-input-from-file file
    (λ _ (for/list ([ln (in-lines)])
           (read (open-input-string (string-replace ln "," " ")))))))


(define (main)
  (let* ([input (read-from-file "advent5_input.txt")]
         [updater1 add1]
         [updater2 (λ (x) (if (>= x 3)
                              (sub1 x)
                              (add1 x)))]
         [result1 (break-free input updater1)]
         [result2 (break-free input updater2)])
    (displayln `(Part1: ,result1))
    (displayln `(Part2: ,result2))))
    
    
(main)

