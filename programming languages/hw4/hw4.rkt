
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;;graphics

;; test
(define ones (lambda () (cons 1 ones)))
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))
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

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (letrec
          ([ans (s)])
        (cons (car ans) (stream-for-n-steps (cdr ans) (- n 1))))))

(define funny-number-stream
  (letrec
      ([fhelper (lambda (x) (if (= 0 (remainder x 5)) (* -1 x) x))]
       [f (lambda (x) (cons (fhelper x) (lambda () (f (+ 1 x)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec
      ([pic (list "dan.jpg" "dog.jpg" "dog2.jpg" "curry.jpg")]
       [fget (lambda (x) (remainder x 4))]
       [f (lambda (x) (cons (car (list-tail pic (fget x))) (lambda () (f (+ 1 x)))))])
    (lambda () (f 0))))

(define (stream-add-zero s)
  (letrec
      ([f (lambda (stream) (cons (cons 0 (car (stream))) (lambda () (f (cdr (stream))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec
      ([xl (length xs)]
       [yl (length ys)]
       [fh (lambda (x y) (remainder x y))]
       [f (lambda (x y) (cons (cons (car (list-tail xs (fh x xl))) (car (list-tail ys (fh y yl))))
                              (lambda () (f (+ 1 x) (+ 1 y)))))])
    (lambda () (f 0 0))))

(define (vector-assoc v vec)
  (letrec
      ([vl (vector-length vec)]
       [f (lambda (x) (cond [(> x (- vl 1)) #f]
                            [#t (letrec
                                    ([vn (vector-ref vec x)])
                                (if (and (pair? vn) (= (car vn) v)) vn (f (+ 1 x))))]))])
  (f 0)))

(define (cached-assoc xs n)
  (letrec
      ([memo (make-vector n #f)]
       [pos 0]
       [f (lambda (x)
          (let ([ans (vector-assoc x memo)])
            (if ans
                ans
                (let ([new-ans (assoc x xs)])
                  (if new-ans
                    (begin (vector-set! memo pos x)
                           (set! pos (remainder (+ 1 pos) n))
                            new-ans)
                    #f)))))])
    f))
