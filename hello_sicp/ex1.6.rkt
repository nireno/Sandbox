#lang sicp

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