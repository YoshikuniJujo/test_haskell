(define (pascal m n)
	(cond	((= n 0) 1)
		((= n m) 1)
		(else (+
			(pascal (- m 1) (- n 1))
			(pascal (- m 1) n)))))
