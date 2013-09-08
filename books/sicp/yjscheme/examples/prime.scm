(load "examples/tools.scm")

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next t)
  (if (= t 2)
    3
    (+ t 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
