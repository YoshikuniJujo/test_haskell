(define (expmod base exp m)
  	(cond	((= exp 0) 1)
		((even? exp)
		 	(define em (expmod base (/ exp 2) m))
		 	(define sq (remainder (square em) m))
			(if (and
				(not (or (= em 1) (= em (- m 1))))
				(= sq 1))
				0
		 		sq))
		(else (remainder (* base (expmod base (- exp 1) m )) m))))

(define (even? n) (= (remainder n 2) 0))

(define (square x) (* x x))

(define (miller-rabin-test n)
	(define (try-it a)
		(= (expmod a (- n 1) n) 1))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
	      ((miller-rabin-test n) (fast-prime? n (- times 1)))
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
