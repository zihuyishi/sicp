(define (even? x)
  (= (remainder x 2) 0))

(define (fast-exp b n)
  (define (fast-iter a b m)
    (if (= m 0)
	a
	(if (even? m)
	    (fast-iter a (* b b) (/ m 2))
	    (fast-iter (* a b) b (- m 1)))))

  (fast-iter 1 b n)))
    
    
