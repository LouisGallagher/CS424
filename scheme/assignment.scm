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
			((null? s2) (cons s1 null))
			((set-contains? s1 (car s2)) (set-union s1 (cdr s2)))
			(else (set-union (list s1 (car s2)) (cdr s2))))))

