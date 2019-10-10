(define (fast-mul a b) (fast-mul-iter a b 0))

(define (fast-mul-iter a counter sum)
	(cond	((= counter 0) sum)
		((even? counter) (fast-mul-iter (double a) (/ counter 2) sum))
		(else (fast-mul-iter a (- counter 1) (+ a sum)))))

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (even? n) (= (remainder n 2) 0))
