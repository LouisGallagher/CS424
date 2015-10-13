;;;;;;;;;;;;;;;;;;;;Some stuff on tail recursion;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;add up sqrt of all primes between n1 and n2;;;;;;;;;
;;Question: is this code tail recursive and if not can we make it so?
;;Answere: 


(define prime? (lambda (n i)
	(cond 
		((> i (sqrt n)) #t)
		(else 
			(cond 
				((equal? (modulo n i) 0) #f)
				(else (prime? n (+ i 1)))
				  )))))


(define ssp (lambda (n1 n2)
	(cond
		 ((> n1 n2) 0);;if n1 > n2
		 ((prime? n1 2) (+ (sqrt n1) (ssp (+ 1 n1) n2)));;otherwise
		 (else (ssp (+ n1 1) n2 ))
		)))



