(load "examples/tools.scm")

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1  v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(try next))))
  (try first-guess))

(define (fixed-point-dump f first-guess)
  (define (close-enough? v1  v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	next
	(try next))))
  (try first-guess))

(define (x^x=1000 x)
  (/ (log 1000) (log x)))

(define (x^x=1000avg x)
  (average x (x^x=1000 x)))
