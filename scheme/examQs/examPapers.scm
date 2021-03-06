;; Takes two lists of the same lenght as arguments. The second list is a list of non-negative integers.
;; Returns another list which is the elements of the first list in reverse order such that each element is repeated k number of times
;; where k is specified in the corresponding element of the second list.

(define reverse-with-count
	(lambda (l c)
		(cond
			((null? l) null)
			((not (list? l))
				(if (< 0 c) (cons l (reverse-with-count l (- c 1))) null)) 
			(else (append (reverse-with-count (cdr l) (cdr c)) (reverse-with-count(car l) (car c)))))))


;; Takes a list of xs and a predicate p as parameters,
;; returns a list of those xs that immediately follow an element for which p is true 

(define after-filter
 (lambda (xs p)
 	(cond
 		((null? (cdr xs)) null)
 		((p (car xs)) (cons (cadr xs) (after-filter (cdr xs) p)))
 		(else (after-filter (cdr xs) p)))))


(define add-numbers
	(lambda (sexpr)
		(cond
			((null? sexpr) 0)
			((number? sexpr) sexpr)
			(else (+ (add-numbers (car sexpr)) (add-numbers (cdr sexpr)))))))


(define deep-fetch
	(lambda (p s-exp)
		(cond
			((null? s-exp) s-exp)
			((list? (car s-exp)) (append (deep-fetch p (car s-exp)) (deep-fetch p (cdr s-exp))))
			((p (car s-exp)) (cons (car s-exp) (deep-fetch p (cdr s-exp))))
			(else (deep-fetch p (cdr s-exp))))))