(define (sum term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a )))))
	(iter a 0))

(define (cube x) (* x x x))

(define (simpsons f a b n)
	(define h (/ (- b 0.0 a) n))
	(define (y k) (f (+ a (* k h))))
	(define (term k) (+ (* 2 (y k)) (* 4 (y (+ k 1)))))
	(define (next k) (+ k 2))
	(* (/ h 3.0) (+ (- (sum term 0 next (- n 2)) (y 0)) (y n))))
