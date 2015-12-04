(define (binbou n)
  	(if (< n 1)
		""
		(string-append (binbou (- n 1)) (x->string n) "円ちょうだい。")))

(binbou 100)
