(load "examples/tools.scm")

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
	(cond ((positive? test-value)
	       (search f neg-point midpoint))
	      ((negative? test-value)
	       (search f midpoint pos-point))
	      (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))
