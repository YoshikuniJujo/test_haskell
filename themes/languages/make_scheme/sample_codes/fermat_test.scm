(define (expmod base exp m)
  	(cond	((= exp 0) 1)
		((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
		(else (remainder (* base (expmod base (- exp 1) m )) m))))

(define (even? n) (= (remainder n 2) 0))

(define (square x) (* x x))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
	      ((fermat-test n) (fast-prime? n (- times 1)))
	      (else false)))

(define false #f)

(define true #t)

(define (test-all? n)
	(define (try-it a)
		(= (expmod a n n) a))
	(define (rec-test t)
		(cond	((> t (- n 1)) true)
			((not (try-it t)) false)
			(else (rec-test (+ t 1)))))
	(rec-test 1))
