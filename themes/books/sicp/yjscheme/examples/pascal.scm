(define (pascal y x)
  (if (or (= x 0) (= x y))
    1
    (+ (pascal (- y 1) (- x 1))
       (pascal (- y 1) x))))
