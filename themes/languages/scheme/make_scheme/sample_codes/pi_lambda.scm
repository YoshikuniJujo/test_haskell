(define (sum-integers a b)
  	(if (> a b)
		0
		(+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  	(if (> a b)
		0
		(+ (cube a) (sum-cubes (+ a 1) b))))

(define (cube x) (* x x x))

(define (pi-sum a b)
	(sum	(lambda (x) (/ 1.0 (* x (+ x 2))))
		a
		(lambda (x) (+ x 4))
		b))

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (identity x) x)

(define (integral f a b dx)
	(*	(sum	f
			(+ a (/ dx 2.0))
			(lambda (x) (+ x dx))
			b)
		dx))


(define (simpsons f a b n)
	(define h (/ (- b 0.0 a) n))
	(define (y k) (f (+ a (* k h))))
	(define (term k) (+ (* 2 (y k)) (* 4 (y (+ k 1)))))
	(define (next k) (+ k 2))
	(* (/ h 3.0) (+ (- (sum term 0 next (- n 2)) (y 0)) (y n))))
