(define (make-interval a b) (cons a b))
(define (upper-bound x)
  (let ((a (car x))
	(b (cdr x)))
    (if (> a b)
	a
	b)))

(define (lower-bound x)
  (let ((a (car x))
	(b (cdr x)))
    (if (< a b)
	a
	b)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (* c (- 1 p))
		 (* c (+ 1 p))))

(define (percent i)
  (let ((l (lower-bound i))
	(u (upper-bound i)))
    (/ (- u l) (+ u l))))



;;;;2.14-2.16;;;;;;
;个人见解，电阻公式相当于(x, y)的二元函数。
;而二元函数在给定区间求极值，并不一定在x或y的上下界
;在算(/ (* r1 r2) (+ r1 r2))时，加法和乘法取得极值的点并不相同
;而(/ 1 (+ (/ 1 r1) (/ 1 r2)))反倒是在极值点取得极值，所以结果必然不同
;更多可以参考二元函数求极值的问题
