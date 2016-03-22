(define (sqr3 x)
  (* x x x))

(define (good-enough? guess x)
  (< (abs (- (sqr3 guess) x)) 0.001))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (sqrt3-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt3-iter (improve guess x)
		  x)))

(define (sqrt3 x)
  (sqrt3-iter 1.0 x))


(define (subject-1 n)
  (if (< n 3)
      n
      (+
       (subject-1 (- n 1))
       (* 2 (subject-1 (- n 2)))
       (* 3 (subject-1 (- n 3))))))

(define (subject-2 n)
  (define (sub-sum a b c)
    (+ c (* 2 b) (* 3 a)))
  (define (sub-iter a b c count)
    (if (< count n)
	(sub-iter b c (sub-sum a b c) (+ count 1))
	(sub-sum a b c)))
  (if (< n 3)
      n
      (sub-iter 0 1 2 3)))
