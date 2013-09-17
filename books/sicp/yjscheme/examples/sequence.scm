(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
	      nil
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
	      nil
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))

(define (fib n)
  (define (f k a b)
    (if (= k 0)
      a
      (f (- k 1) b (+ a b))))
  (f n 0 1))
