(define (abs x)
	(if (< x 0)
		(- x)
		x))

(define (>= x y)
	(not (< x y)))
