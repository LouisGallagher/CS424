
;;;;;;;;;;;;;;;;;;;some basic functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hypot
	(lambda (x y)
		(sqrt 
			(+ (expt x 2) (expt y 2)))))

(define fact
	(lambda (n)
		(if (<= n 0)
			1
			(* n (fact (- n 1))
			))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;Some data structures;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-pair   ;; a procedure that takes two arguments and returns another  
	(λ (x y)		;; procedure for manipulating those arguments
		(λ (z)		;; <--the procedure being returned 
			(if z x y))))

(define pair (make-pair 1 2)) ;; this really evaluates to a lambda expression 

(define fst-elem (pair #t)) ;;fst-elem=1, would equal 2 if we passed in #f, remember pair is a lambda

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;A second go at pairs;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-pair2      
	(lambda (x y)      ;; a procedure that returns another procedure
		(lambda (z)    ;; the returned procedure takes a lambda as an argument
			(z x y)))) ;; and calls the lambda, z, on two arguments

(define get-fst2				;; p is a pair, which evaluates to a lambda
	(lambda (p)                 ;;get-fst2 of p calls p with a lambda 
		(p (lambda (x y) x))))    ;;that takes two args and returns the first

(define get-snd2				;; p is a pair, which evaluates to a lambda
	(lambda (p)                 ;;get-snd2 of p calls p with a lambda 
		(p (lambda (x y) y))))     ;;that takes two args and returns the second

(define pair (make-pair2 1 2))   ;; a pair this evals to a lambda that takes one lambda as an arg
(define fst(get-fst2 pair))	     ;; evaluates to the first element of the pair 
(define snd(get-snd2 pair)) 	 ;; evaluates to the second element of the pair 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;lists;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define l (list 1 2 3 4 5))				  ;;make a list
(define top(car l ))                      ;;returns first element in the list
(define rest(cdr l))					  ;;returns rest of the list
(define tailtail(cdr(cdr l)))			  ;; returns the tail of the tail of l


;;using cons to make a list 
(cons 1 (cons 2 (cons 3 (cons 4 null))))  ;;cons builds pairs, if one of the pair is another pair it becomes a list
										  ;;thus list is shorthand for the above cons construct

;;syntactic sugar for taking the top
(define bob (list 1 2 3 4 5 6 7 8 9))
(define t (cadr bob))              		  ;; same as (car(cdr bob))
(define t2 (caddr bob))					  ;; same as (car (cdr (cdr bob))) add  i d's to take car of the ith tail 
(define longt2 (car (cdr (cdr bob))) )

;;syntactic sugar for taking the tail
(define something (cddr bob))     		  ;;same as (cdr (cdr bob)) add  i d's to take cdr of the ith tail 
(define somethingelse (cdr (cdr bob)))    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Recursion;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			

;;see tail recursion for optimisation 
(define my-length
 (lambda (x)
 	(if (null? x)    
 		0            
 		(+ 
 			(my-length (cdr x))   ;;length is 1 + length of tail of current list
 			1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Symbols;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(quote car)   ;; yeah that's symbols 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Some more functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define factorial 
	(lambda (x)
	 (if (zero? x)
	 	1
	 	(* (factorial (- x 1)) x ))))