(define (product term a next b)
	(if (> a b)
		1
		(* (term a) (product term (next a) next b))))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (pi-product a b)
	(define (pi-term x) (/ (* (- x 1.0) (+ x 1)) (* x x)))
	(define (pi-next x) (+ x 2))
	(product pi-term a pi-next b))
