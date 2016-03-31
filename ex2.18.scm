(define (reverse l)
  (define (iter r items)
    (if (null? items)
	r
	(iter (cons (car items) r) (cdr items))))
  (iter '() l))
