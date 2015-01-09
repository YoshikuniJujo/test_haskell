(define (abs x) (if (< x 0) (- x) x))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (odd? n) (logbit? 0 n))
(define (even? n) (not (odd? n)))
(define (newline) (display "\n"))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (append lst1 lst2)
  (if (null? lst1)
    lst2
    (cons (car lst1) (append (cdr lst1) lst2))))

(define (map1 proc items)
  (if (null? items)
    ()
    (cons (proc (car items))
	  (map1 proc (cdr items)))))

(define (or-list bs)
  (if (null? bs)
    #f
    (or (car bs) (or-list (cdr bs)))))

(define (map proc . lss)
  (define (map-rec fun lss)
    (if (or-list (map1 null? lss))
      '()
      (cons (apply fun (map1 car lss))
	    (map-rec fun (map1 cdr lss)))))
  (map-rec proc lss))

(define (for-each proc items)
  (cond ((null? items) (undefined))
	(else (proc (car items)) (for-each proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))
