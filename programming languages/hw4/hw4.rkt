
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;;graphics


;; put your code below

(define (sequence low high stride)
  (if (> low high)
  null
  (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (st) (string-append st suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(> 0 n) (error  "list-nth-mod: negative number")]
        [(null? xs)  (error "list-nth-mod: empty list")]
        [#t (letrec
               ([r (remainder n (length xs))]
                )
              (car (list-tail xs r)))]))
  
  