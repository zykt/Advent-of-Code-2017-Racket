#lang racket



(define (main)
  (let* ([input (file->lines "advent4_input.txt")]
         [no-duplicates (λ (s) (not (check-duplicates (string-split s))))]
         [sort-string (λ (s) (list->string (sort (string->list s) char<?)))]
         [no-anagrams (λ (s) (not (check-duplicates (map sort-string (string-split s)))))]
         [result1 (count no-duplicates input)]
         [result2 (count no-anagrams input)]
         [s2 "jpsr wwex yjgdj fqah wrmmw nyrnw hcomcgv teajmu emw zrraid"])
    (displayln `(Part1: ,result1))
    (displayln `(Part2: ,result2))
    
    
(main)