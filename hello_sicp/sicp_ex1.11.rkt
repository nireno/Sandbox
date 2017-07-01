#lang sicp

(define (f-rec n)
  (cond ((< n 3) n)
        (else (+ (f-rec (- n 1))
                 (* 2 (f-rec (- n 2)))
                 (* 3 (f-rec (- n 3)))))))


(define (f n) 
  (if (< n 3) n
      (f-iter 0 1 2 3 n)))
  
(define (f-iter fi-3 fi-2 fi-1 i n)
  (define fi (+ fi-1 (* 2 fi-2) (* 3 fi-3)))
  (cond ((= i n) fi)
        (else (f-iter fi-2 fi-1 fi (inc i) n))))