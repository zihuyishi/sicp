(define (add a b)
  (+ a b))

(define (double a)
  (* 2 a))

(define (halve a)
  (/ a 2))

(define (dec n)
  (add n -1))

(define (even? n)
  (= (remainder n 2) 0))
  
(define (fast-mul a b)
  (define (fast-iter ans x y)
    (cond
     ((= y 0) ans)
     ((even? y) (fast-iter ans (double x) (halve y)))
     (else (fast-iter (add ans x) x (dec y)))))
  (fast-iter 0 a b))
