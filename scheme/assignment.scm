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
			((and (null? s1) (null? s2)) null)
			((null? s2) (cons (car s1) (set-union (cdr s1) s2)))
			((null? s1) (cons (car s2) (set-union  s1 (cdr s2))))
			((set-contains? s1 (car s2)) (set-union  s1 (cdr s2)))
			(else (cons (car s2) (set-union  s1 (cdr s2)))))))

;; returns the set that is the intersection of s1 and s2
(define set-intersection
	(lambda (s1 s2)
		(cond
			((null? s1) null)
			((set-contains? s2 (car s1)) (cons (car s1) (set-intersection (cdr s1) s2)))
			(else (set-intersection (cdr s1) s2)))))

;; returns s1\s2 i.e. those elements in s1 that are not also in s()2
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


;;;;;;;;;;;;;;;;;;;;;;;;; lambda calculus manipulation;;;;;;;;;;;;;;;;;;;;;

;; Gets the set of bound variables in a lambda calculus (sub)expression 
(define bound-variables
	(λ (e)
		(cond
			((null? e) null)
			((list? (car e)) (set-union (bound-variables(car e)) (bound-variables (cdr e))))
			((equal? (car e) 'λ) (set-union (list (cadr e)) (bound-variables (cddr e))))
			(else (bound-variables (cdr e))))))

;; Gets the set of free variables in a lambda calculus (sub)expression 
(define free-variables
	(lambda (e)
		(cond
			((list? e)
				(cond
					((null? e) null)
					((or (equal? (car e) 'lambda) (equal? (car e) 'λ)) (set-difference (free-variables (cddr e)) (list (cadr e))))		    		
		    		(else (set-union (free-variables (car e)) (free-variables (cdr e))))))
		    (else (cons e null)))))

(define replace
	(λ (e1 e2 x)
		(cond 
			((list? e1)
				(cond
					((null? e1) null)
					((equal? (car e1) 'λ) e1)
					(else (cons (replace (car e1) e2 x) (replace (cdr e1) e2 x)))))
			((equal? e1 x) e2)
			(else e1))))

(define β-reduce
	(λ (e)
		(cond
			((and 
				(pair? e)
				 (and 
				 	(equal? (caar e) 'λ) 
				 	(null? (set-intersection (bound-variables (cddar e)) (free-variables (cdr e)) )))) 
			(replace (cddar e) (cdr e) (cadar e)))                                                                                                                                   
			(else #f))))

;;;;;;;;;;;;;;;;;;;;;;;end lambda calc stuff ;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to come 