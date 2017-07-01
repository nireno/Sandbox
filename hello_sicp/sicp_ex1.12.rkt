#lang sicp

(define (py-tri row col)
  (cond ((or (= col 1) (= col row)) 1)
        (else (+ (py-tri (- row 1) col)
                 (py-tri (- row 1) (- col 1))))))