(define (mul a b)
	(if (= b 0)
		0
		(+ a (mul a (- b 1)))))

(define (fast-mul a b)
	(cond	((= b 0) 0)
		((even? b) (double (fast-mul a (/ b 2))))
		(else (+ a (fast-mul a (- b 1))))))

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (even? n) (= (remainder n 2) 0))
