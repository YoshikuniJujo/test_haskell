(load "examples/tools.scm")

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (define e (expmod base (/ exp 2) m))
	 (define se (remainder (square e) m))
	 (if (and (= se 1) (not (= e 1)) (not (= e (- m 1))))
	   0
	   se))
	(else
	  (remainder (* base (expmod base (- exp 1) m))
		     m))))

(define (mirror-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((mirror-rabin-test n) (fast-prime? n (- times 1)))
	(else false)))
