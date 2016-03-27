(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((fix-n (if (< (/ n d) 0)
		   (- (abs n))
		   (abs n)))
	(fix-d (abs d))
	(g (abs (gcd n d))))
    (cons (/ fix-n g) (/ fix-d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


