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

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))
