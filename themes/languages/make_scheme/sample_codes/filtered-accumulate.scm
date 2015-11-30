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

(define (gcd a b)
	(if (= b 0)
	  	a
		(gcd b (remainder a b))))

(define (filtered-accumulate combiner null-value predicate term a next b)
	(cond	((> a b) null-value)
		((predicate a) (combiner
			(term a)
			(filtered-accumulate
			  	combiner
				null-value
				predicate
				term
				(next a)
				next b)))
		(else (filtered-accumulate
			combiner
			null-value
			predicate
			term
			(next a)
			next b))))

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (sum-ps a b)
	(filtered-accumulate + 0 prime? square a inc b))

(define (product-coprime n)
	(define (coprime a) (= 1 (gcd a n)))
	(filtered-accumulate * 1 coprime identity 1 inc (- n 1)))
