 (define foo
 (lambda (f xs ys)
 (if (null? xs)
 xs
 (cons (f (car xs))
 (foo f ys (cdr xs)))))) 