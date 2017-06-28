#lang sicp

(define (f x) 
  (if (< x 3) x
      (f-iter 1 2 3 3 x)))
  

(define (f-iter n1 n2 acc i x)
  (cond ((= i x) acc)
        (else (f-iter n2 acc (+ n1 n2 acc) (inc i) x))))

