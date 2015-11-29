(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (add a b)
	(if (= a 0)
		b
		(inc (+ (dec a) b))))

(define (addadd a b)
	(if (= a 0)
		b
		(addadd (dec a) (inc b))))
