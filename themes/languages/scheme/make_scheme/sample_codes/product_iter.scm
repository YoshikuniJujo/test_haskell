(define (product term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* result (term a)))))
	(iter a 1))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (pi-product a b)
	(define (pi-term x) (/ (* (- x 1.0) (+ x 1)) (* x x)))
	(define (pi-next x) (+ x 2))
	(product pi-term a pi-next b))
