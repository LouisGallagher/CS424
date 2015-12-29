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

