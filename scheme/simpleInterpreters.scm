;;; Little Expression in BNF
;;; <le> ::= <number>
;;;        | '(+' <le> <le> ')'
;;;        | '(*' <le> <le> ')'
;;;
;;; A simple interpreter for Little expressions 

(define le-eval
	(lambda (le)
		(if (number? le)
			le
			(if (equal? (car le) '+)
				(+ (le-eval (cadr le)) (le-eval (caddr le)) )
				(if (equal? (car le) '*)
					(* (le-eval (cadr le)) (le-eval (caddr le)))
					(error "bad little expression" le))))))


;;example for le = '(+ 1 2)
;;> (define le '(+ 1 2)) ;;applies quote operation to all elements in (), so it's equivalent of (quote (+ 1 2))
;;> (le-eval le)
;;3	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;quote, an aside;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define single (quote symbol))    ;; a single symbol 

(define mult (quote (a b c d)))	  ;;a list of symbols 	
(define top (car mult)) 		  ;;should be a 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;Rewriting LE interpreter with cond;;;;;;;;;;;;;;;;;;;;;;;

(define le-eval2
	(lambda (le)
		(cond
			((number? le) le) 
			((equal?  (car le) '+) (+ (le-eval2 (cadr le)) (le-eval2 (caddr le))))
			((equal?  (car le) '*) (* (le-eval2 (cadr le)) (le-eval2 (caddr le))))
			(else (error "bad le " le)) )))

;;cond is of the form:
;; (cond (<test1> <e1>)
;;		  .
;;		  .
;;       (<testn> <en>)
;;
;;       (else  <en+1>))

;; cond is a macro that aims to make complex if-else structures tidier 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;Taking derivatives of less little expressions;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An LLE can be an LE or the symbol x 
;;Derivatives refer to the Newton/Leibnitz method, e.g. :
;; 						d/dx u*v = u * dv/dx + du/dx * v
;;

(define d/dx2
	(lambda (e)
		(cond ((number? e) 0)      ;; d/dx of constant is 0
			  ((equal? e 'x) 1)    ;; d/dx of x is 1
			  ((equal? (car e) '+ ) ;; the derivative of a sum is the sum of the derivatives
			  		(list '+ (d/dx (cadr e) ) (d/dx (caddr e)) ))
			  ((equal? (car e) '*)   ;;leibnitz rule, see above
			  	(list '+ 
			  		(list '* (cadr e) (d/dx (caddr e)))     ;; u * dv/dx
			  		(list '* (caddr e) (d/dx (cadr e))) ))  ;; v * du/dx
			  (else (error "bad LLE" e)))))

;;This code is ugly and lacks modularity 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;Taking derivatives; clean code;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;first attempt at cleaning up the code
;;main mechanism used to do this is to return a lamda for the recursive case
;;the last thing the procedure does is call the lambda, which calls the procedure 


(define d/dx3
	(lambda (e)
		(cond ((number? e) 0)  ;; first two conditions same as above
			  ((equal? e 'x) 1)
			  (else
			    ((lambda (u v)  ;; return lambda that takes derivative of v and u 
			    	(cond 
			    		((equal? (car e) '+) (list '+ (d/dx u) (d/dx v)))
			    		((equal? (car e) '*) (list '+ (list '* u (d/dx v)) (list '* v (d/dx u)) ))
			    		(else (error "bad LLE" e)))
			    	)
			    (cadr e) (caddr e))))))   ;; call the lambda with params v and u

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;Taking derivatives; even cleaner code(supposedly);;;;;;;;;;;;;;;;;;;;
;;Main mechanism used here to clean up the code is the use  of the 'let' construct.
;;This, as in the example above, allows use to eliminate redundant code, i.e. the 
;;pullinfg out of v and u from e 
;;

(define d/dx
	(lambda (e)
		(cond
			((number? e) 0)
			((equal? e 'x) 1)
			(else 
				(let ((u (cadr e)) (v (caddr e)))   ;; sets u = (cadr e), v = (caddr e)
				(cond
					((equal? (car e) '+) (list '+ (d/dx u) (d/dx v)))
					((equal? (car e) '*) (list '+ (list '* u (d/dx v)) (list '* v (d/dx u))))
					(else (error "bad LLE" e))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;Yet another attempt at cleaning up the derivative code;;;;;;;;;;;;;;;;;;;;;;;;;
;;The main mechanism used here is table driven code
;; also handles arbitrary symbols

(define d/dx 
	(lambda (e x)
		(cond 
			((number? e) 0)
			(((equal? e x) 1))
			((symbol? e) 0)    ;; d/dx(y)  = 0
			((pair? e)
				(let ((f (car e))))
				 (apply (lookup f dtable)     ;;resolves to a function
				 	((append (cdr e) (map (λ (u) (d/dx u x)) (cdr e)))))) ;; takes this list as arguments
			(else (error "Bad LLE " e)))))

(define dtable (list
	(list '* (lambda (u v du dv)(lle+ (lle* u dv) (lle* v du))))
	(list '+ (lambda (u v du dv)(lle+ du dv)))))


(define lookup
	(lambda (x alist)
	(if (equal? x (caar alist))   ;;for nested lists caar = the first element of the first list in the list
		(cadar alist)			  ;;for nested lists cadar = the tail of the first list in the list
		(lookup x (cdr alist))))) ;;else look in the next list in the list 

(define lle+ 
	(lambda (e1 e2)
		(cond
			((and (number? e1) (number? e2)) (+ e1 e2))
			((zero? e1) e2)
			((zero? e2) e1)
			(else (list '+ e1 e2)))))

(define lle*
	(lambda (e1 e2)
		(cond 
			((or (zero? e1) (zero? e2)) 0)  ;; if either are zero the result is zero 
			((equal? e1 1) e2)              ;; 1 = identity
			((equal? e2 1) e1)
			((else (list '* e1 e2))))))     ;;
