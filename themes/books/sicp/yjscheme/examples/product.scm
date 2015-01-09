(load "examples/tools.scm")

(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

(define (pi/4 n)
  (define (pi-term k)
    (/ (* k (+ k 2)) (square (+ k 1))))
  (define (pi-next x)
    (+ x 2))
  (product pi-term 2.0 pi-next n))

(define (product-iter term a next b)
  (define (iter k result)
    (if (> k b)
      result
      (iter (next k) (* (term k) result))))
  (iter a 1))

(define (pi/4-iter n)
  (define (pi-term k)
    (/ (* k (+ k 2)) (square (+ k 1))))
  (define (pi-next x)
    (+ x 2))
  (product-iter pi-term 2.0 pi-next n))
