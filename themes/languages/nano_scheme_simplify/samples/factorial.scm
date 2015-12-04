(define (factorial n) (if (< n 1) 1 (* n (factorial (- n 1)))))

(factorial 10)
(factorial 100)
