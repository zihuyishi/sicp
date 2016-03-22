(define (even? x)
  (= (remainder x 2) 0))

(define (fast-exp b n)
  (define (fast-iter a m)
    (if (= m 0)
	a
	(if (even? m)
	    (fast-iter (* a a) (/ m 2))
	    (fast-iter (* a b) (- m 1)))))

  (fast-iter b (- n 1)))
    
    
