(define (square x) (* x x))

(define (sum-squares x y) (+ (square x) (square y)))

(define (sum-bigs x y z) (cond
	((and (< x y) (< x z)) (sum-squares y z))
	((< y z) (sum-squares z x))
	(else (sum-squares x y))))
