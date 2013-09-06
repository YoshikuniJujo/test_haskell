(define (fr n)
  (if (< n 3)
    n
    (+ (fr (- n 1))
       (* 2 (fr (- n 2)))
       (* 3 (fr (- n 3))))))

(define (fi n)
  (define (f a b c count)
    (if (= count 0)
      c
      (f (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (f 2 1 0 n))
