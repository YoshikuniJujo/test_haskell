(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (sim-term x)
    (+ (f x) (* 4 (f (+ x h))) (f (+ x (* 2 h)))))
  (define (sim-next x)
    (+ x (* 2 h)))
  (* (/ h 3) (sum sim-term a sim-next b)))

(define (sum-iter term a next b)
  (define (iter x result)
    (if (> x b)
      result
      (iter (next x) (+ (term x) result))))
  (iter a 0))
