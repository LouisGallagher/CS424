;;;;;;;;;;;;;;;;;;;;;;;;;finger exercises;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define set-contains?
	(lambda (s e)
		(cond
			((null? s) #f)
			((equal? (car s) e) #t)
			(else (set-contains? (cdr s) e)))))

(define set-cardinally 
	(lambda (s)
		(if (null? s)
			0 
			(+ 1 (set-cardinally (cdr s))))))

(define set-union
	(lambda (s1 s2)
		(cond
			((null? s2) s1 )
			((null? s1) s2 )
			((set-contains? s1 (car s2)) (cons (car s1) (set-union (cdr s1) (cdr s2))))
			(else (cons (car s1) (cons (car s2) (set-union (cdr s1) (cdr s2)))))))) 

(define set-intersection
	(lambda (s1 s2)
		(cond
			((null? s1) null)
			((set-contains? s2 (car s1)) (cons (car s1) (set-intersection (cdr s1) s2)))
			(else (set-intersection (cdr s1) s2)))))

(define set-difference
	(lambda (s1 s2)
		(cond
			((null? s1) s2)
			((null? s2) s1)
			((set-contains? s1 (car s2)) (set-difference s1 (cdr s2)))
			(else (cons (car s2) (set-difference s1 (cdr s2)))))))