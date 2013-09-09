(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
	      (accumulate-rec combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter k result)
    (if (> k b)
      result
      (iter (next k) (combiner (term k) result))))
  (iter a null-value))

(define (filtered-accumulate combiner null-value term predicate a next b)
  (cond ((> a b) null-value)
	((predicate a)
	 (combiner (term a)
		   (filtered-accumulate
		     combiner null-value term predicate (next a) next b)))
	(else (filtered-accumulate
		combiner null-value term predicate (next a) next b))))
