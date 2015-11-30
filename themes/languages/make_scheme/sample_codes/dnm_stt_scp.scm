(define (fun f x)
	(f x))

(define (outer x)
	(define (inner y) (+ x y))
	(fun inner 8))

(define (outer-two z)
	(define (inner y) (+ z y))
	(fun inner 8))
