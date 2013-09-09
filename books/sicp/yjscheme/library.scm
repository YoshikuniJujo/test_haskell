(define (abs x) (if (< x 0) (- x) x))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (odd? n) (logbit? 0 n))
(define (even? n) (not (odd? n)))
(define (newline) (display "\n"))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
