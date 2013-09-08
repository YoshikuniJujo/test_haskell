(define (abs x) (if (< x 0) (- x) x))
(define (odd? n) (logbit? 0 n))
(define (even? n) (not (odd? n)))
(define (newline) (display "\n"))
