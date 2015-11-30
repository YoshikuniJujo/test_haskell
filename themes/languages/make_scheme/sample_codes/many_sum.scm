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
  	(if (> a b)
		0
		(+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (pi-sum-two a b)
	(define (pi-term x) (/ 1.0 (* x (+ x 2))))
	(define (pi-next x) (+ x 4))
	(sum pi-term a pi-next b))

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (identity x) x)

(define (integral f a b dx)
	(define (add-dx x) (+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpsons f a-s b-s n)
	(define h (/ (- b-s 0.0 a-s) n))
	(define (y k) (f (+ a-s (* k h))))
	(define (term-s k) (+ (* 2 (y k)) (* 4 (y (+ k 1)))))
	(define (next-s k) (+ k 2))
	(* (/ h 3.0) (+ (- (sum term-s 0 next-s (- n 2)) (y 0)) (y n))))
