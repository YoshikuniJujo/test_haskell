(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (if (not (pair? mobile))
    mobile
    (let ((left (left-branch mobile))
	  (right (right-branch mobile)))
      (+ (total-weight (branch-structure left))
	 (total-weight (branch-structure right))))))

(define mobile-example
  (make-mobile (make-branch 10 3)
	       (make-branch 20 (make-mobile (make-branch 5 7)
					    (make-branch 8 3)))))

(define balanced-example
  (make-mobile (make-branch 10 44)
	       (make-branch 20 (make-mobile (make-branch 3 16)
					    (make-branch 8 6)))))

(define (balanced? mobile)
  (if (not (pair? mobile))
    true
    (let ((left (left-branch mobile))
	  (right (right-branch mobile)))
      (let ((ll (branch-length left))
	    (rl (branch-length right))
	    (ls (branch-structure left))
	    (rs (branch-structure right)))
	(and (= (* ll (total-weight ls)) (* rl (total-weight rs)))
	     (balanced? ls)
	     (balanced? rs))))))
