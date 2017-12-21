#lang racket

(define (shift-left n lst)
  (append (drop lst n) (take lst n)))

(define (sum-matching-numbers lst1 lst2)
  (for/sum ([i lst1]
            [j lst2]
            #:when (= i j))
    i))

(define (main)
  (define (reader in)
    (let ([ch (read-char in)])
      (if (eof-object? ch)
          eof
          (- (char->integer ch) 48))))
  (define input
    (file->list "advent1_input.txt" reader))
  (display "First half:")
  (display (sum-matching-numbers input (shift-left 1 input)))
  (newline)
  (display "Second half:")
  (display (sum-matching-numbers input (shift-left (/ (length input) 2) input)))
  (newline))

(main)