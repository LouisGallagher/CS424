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

