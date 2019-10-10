(define (cont-frac n d k)
  (define (cf i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (cf (+ i 1))))))
  (cf 1))

(define (cont-frac-iter n d k)
  (define (cf i r)
    (if (< i 1)
      r
      (cf (- i 1) (/ (n i) (+ (d i) r)))))
  (cf k 0))

(define (const1 i) 1)

(define (napier i)
  (if (= (remainder i 3) 2)
    (* (/ (+ i 1) 3) 2)
    1.0))

(define (tan-cf x k)
  (define (n i) (if (= i 1) x (- (* x x))))
  (define (d i) (- (* i 2) 1))
  (cont-frac n d k))
