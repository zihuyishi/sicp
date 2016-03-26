(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (normal-f x)
  (/ (log 1000) (log x)))

(define (damping-f x)
  (+ (/ x 2) (/ (normal-f x) 2)))

(define (normal-guess x)
  (fixed-point normal-f x))

(define (damping-guess x)
  (fixed-point damping-f x))
