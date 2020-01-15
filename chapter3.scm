(define rember
	(lambda (a l)
		(cond
			((null? l) quote ())
			((eq? a (car l)) (cdr l))
			(else (rember a (cdr l))))))

(define firsts
	(lambda (l)
		(cond
			((null? l) (quote ()))
			(else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
	(lambda (new old lat)
		(cond
			((null? lat) (quote ()))
			((eq? old (car lat)) (cons old  (cons new (cdr lat))))
			(else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertR
	(lambda (new old lat)
		(cond
			((null? lat) (quote ()))
			((eq? old (car lat)) (cons new (cons old (cdr lat))))
			(else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
	(lambda (new old lat)
	(cond 
		((null? lat)(quote ()))
		((eq? old (car lat)) (cons new (cdr lat)))
		(else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
	(lambda (new o1  o2 lat)
	(cond 
		((null? lat)(quote ()))
		((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
		(else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
	(lambda (a lat)
		(cond
			((null? lat) (quote ()))
			((eq? a (car lat)) (multirember a (cdr lat)))
			(else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
	(lambda (new old lat)
		(cond
			((null? lat) (quote ()))
			((eq? old (car lat))
				(cons old (cons new (multiinsertR new old (cdr lat)))))
			(else
				(cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multisubst
	(lambda (new old lat)
		(cond
			((null? lat) (quote ()))
			((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
			(else (cons (car lat) (multisubst new old (cdr lat)))))))