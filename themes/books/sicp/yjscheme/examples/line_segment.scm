(load "examples/tools.scm")

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint p1 p2)
  (make-point (average (x-point p1) (x-point p2))
	      (average (y-point p1) (y-point p2))))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

(define (midpoint-segment l)
  (midpoint (start-segment l)
	    (end-segment l)))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))
