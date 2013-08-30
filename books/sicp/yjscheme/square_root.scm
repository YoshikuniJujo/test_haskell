(define (abs x)
  (cond
    ((< x 0) (- x))
    (else x)))

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
	       x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))
