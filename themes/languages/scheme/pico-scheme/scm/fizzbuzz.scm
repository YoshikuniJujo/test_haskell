(define (range init max)
	(cond	((< max init) '())
		(else (cons init (range (+ 1 init) max)))))

(define fizzbuzz1 (lambda (n)
	(cond	((= (modulo n 15) 0) "fizzbuzz")
		((= (modulo n 3) 0) "fizz")
		((= (modulo n 5) 0) "buzz")
		(else (number->string n)))))

(define fizzbuzz (string-append (string-join (map fizzbuzz1 (range 1 100)) "\n") "\n"))

fizzbuzz
