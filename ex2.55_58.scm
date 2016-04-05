;;是否为变量
(define (variable? x) (symbol? x))
;;是否为相同变量
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;;
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))
;;和式第一个变量为+
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
;;被加数
(define (addend s) (cadr s))
;;加数
(define (augend s) (caddr s))
;;乘式就是第一个元素为*的表
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
;;被乘数
(define (multiplier p) (cadr p))
;;乘数
(define (multiplicand p) (caddr p))
;;检测表达式是否等于给定的数
(define (=number? exp num)
  (and (number? exp) (= exp num)))
;;乘幂
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (expbase exp) (cadr exp))
(define (exponent exp) (caddr exp))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
	((=number? b 1) 1)
	(else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))
	((exponentiation? exp)
	 (let ((b (expbase exp))
	       (e (exponent exp)))
	   (make-product (make-product e
				       (make-exponentiation b
							    (make-sum e -1)))
			 (deriv b var))))
	(else
	 (error "unknown expression type -- DERIV" exp))))
	
