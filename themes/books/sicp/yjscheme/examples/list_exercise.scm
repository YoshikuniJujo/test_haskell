(define (last-pair lst)
  (if (null? (cdr lst))
    lst
    (last-pair (cdr lst))))

(define (reverse lst)
  (define (iter o r)
    (if (null? o)
      r
      (iter (cdr o) (cons (car o) r))))
  (iter lst ()))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	  (+ (cc amount
		 (except-first-denomination coin-values))
	     (cc (- amount
		    (first-denomination coin-values))
		 coin-values)))))

(define (no-more? cv) (null? cv))
(define (first-denomination cv) (car cv))
(define (except-first-denomination cv) (cdr cv))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (same-parity x . xs)
  (define (sp na)
    (if (null? na)
      ()
      (let ((n (car na))
	    (ns (cdr na)))
	(if (= (even? x) (even? n))
	  (cons n (sp ns))
	  (sp ns)))))
  (cons x (sp xs)))
