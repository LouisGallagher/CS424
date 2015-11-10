;;;;;;;;;;;;;;;;;;;;;;;;;set functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; return true if the set s contains the element e, false otherwise
(define set-contains?
	(lambda (s e)
		(cond
			((null? s) #f)
			((equal? (car s) e) #t)
			(else (set-contains? (cdr s) e)))))

;; returns true if s1 is a subset of s2, false otherwise
(define is-subset?
	(lambda (s1 s2)
		(cond
			((null? s1) #t)
			((set-contains? s2 (car s1)) (is-subset? (cdr s1) s2))
			(else #f))))

;; returns |s|
(define set-cardinally 
	(lambda (s)
		(if (null? s)
			0 
			(+ 1 (set-cardinally (cdr s))))))

;; returns the set that is the union of s1 and s2
(define set-union
	(lambda (s1 s2)
		(cond
			((null? s2) s1 )
			((null? s1) s2 )
			((set-contains? s1 (car s2)) (cons (car s1) (set-union (cdr s1) (cdr s2))))
			(else (cons (car s1) (cons (car s2) (set-union (cdr s1) (cdr s2)))))))) 

;; returns the set that is the intersection of s1 and s2
(define set-intersection
	(lambda (s1 s2)
		(cond
			((null? s1) null)
			((set-contains? s2 (car s1)) (cons (car s1) (set-intersection (cdr s1) s2)))
			(else (set-intersection (cdr s1) s2)))))

;; returns s1\s2 i.e. those elements in s1 that are not also in s2
(define set-difference
	(lambda (s1 s2)
		(cond
			((null? s1) null)  
			((set-contains? s2 (car s1)) (set-difference (cdr s1) s2))
			(else (cons (car s1) (set-difference (cdr s1) s2))))))

;; determines if two sets are equal i.e. contains the same elements and only those elements
(define set-equal?
	(lambda (s1 s2)
		(and (equal? (set-cardinally s1) (set-cardinally s2)) (is-subset? s1 s2))))

;; applies f to every element of s and collects the results in a new set. f itself should return a set
(define set-map-join
	(lambda (f s)
		(cond
			((null? s) null)
			(else (set-union (f (car s)) (set-map-join f (cdr s)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;end set functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;