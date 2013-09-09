(load "examples/tools.scm")

(define (cont-frac-rec n d k)
  (define (rec x)
    (if (> x k)
      0
      (/ (n x) (+ (d x) (rec (+ x 1))))))
  (rec 1))

(define (cont-frac-iter n d k)
  (define (iter x result)
    (if (= x 0)
      result
      (iter (- x 1) (/ (n x) (+ (d x) result)))))
  (iter k 0))

(define (some n)
  (if (= (remainder n 3) 2)
    (* 2 (+ (quotient n 3) 1))
    1))

(define (tan-cf x k)
  (cont-frac-rec (lambda (i) (if (= i 1) x (- (square x))))
		 (lambda (i) (- (* 2 i) 1))
		 k))
