(define (smallest-divisor n)
  	(find-divisor n 2))

(define (find-divisor n test-divisor)
  	(cond	((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (next test-divisor)))))

(define (next n)
  	(if (= n 2)
	  	3
		(+ n 2)))

(define (divides? a b)
  	(= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? n)
  	(= n (smallest-divisor n)))

(define (search-for-primes n)
	(if (prime? n)
	  	n
		(search-for-primes (+ n 1))))
