(load "examples/prime.scm")
(load "examples/fermat.scm")

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (cond ((fast-prime? n 5)
    (display n)
    (report-prime (- (runtime) start-time))
    (newline))))

(define (report-prime elasped-time)
  (display " *** ")
  (display elasped-time))

(define (<= n m)
  (not (> n m)))

(define (search-for-primes n m)
  (cond ((<= n m) (timed-prime-test n)
		  (search-for-primes (+ n 1) m))))
