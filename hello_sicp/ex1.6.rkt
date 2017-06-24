#lang sicp

(define (average x y) (/ (+ x y) 2))

(define (square x) (* x x) )

(define (good-enough? guess x)
  (if (< (abs (- x (square guess))) 0.001)
      true
      false))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(sqrt-iter 1.0 25)

(define (new-if predicate consequent alternative)
  (cond (predicate consequent)
        (else alternative)))

(new-if (good-enough? 2 3) true false)

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (new-sqrt-iter (improve guess x) x)))

;;;; Exercise 1.6
;;;; Why does if need to be provided as a special form?

;; Alyssa: "Why can't I just define it [if] as an ordinary procedure in terms
;; of cond?"
;; Me: "Alyssa, your interpreter is going to use applicative order evaluation
;; until it dies of exhaustion

;; Example
(sqrt-iter 1.0 1)
;; evaluate sqrt-iter    [1]
(new-if (good-enough? 1.0 1) 1.0 (sqrt-iter (improve 1.0 1) 1))
;; evaluate good-enough? [1]
(new-if #t 1.0 (sqrt-iter (improve 1.0 1) 1))
;; evaluate improve      [1]
(new-if #t 1.0 (sqrt-iter 1.0 1))
;; evaluate sqrt-iter    [2]
(new-if #t 1.0
        (new-if (good-enough? 1.0 1) 1.0 (sqrt-iter (improve 1.0 1) 1)))
;; evaluate good-enough? [2]
(new-if #t 1.0
        (new-if #t 1.0 (sqrt-iter (improve 1.0 1) 1)))
;; evaluate improve      [2]
(new-if #t 1.0
        (new-if #t 1.0 (sqrt-iter 1.0 1)))
;; evaluate sqrt-iter    [3] Ad Infinitum...
(new-if #t 1.0
        (new-if #t 1.0
                (new-if (good-enough? 1.0 1)
                        1.0
                        (sqrt-iter (improve 1.0 1) 1))))


;; The built in if expression selectively evaluates only one of either the
;; consequent or the alternative and returns its value. So in the example above
;; it would have evaluated and returned its consequent 1.0 after good-enough [1]
;; evaluated to true