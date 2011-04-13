(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(define add1 (lambda (x) (+ x 1)))

(define sub1 (lambda (x) (- x 1)))

(define plus
  (lambda (x y)
	(cond
	  ((zero? y) x)
	  (else
		(add1 (plus x (sub1 y)))))))

(define minus
  (lambda (x y)
	(cond
	  ((zero? y) x)
	  (else
		(sub1 (minus x (sub1 y)))))))

(define addtup
  (lambda(tup)
	(cond
	  ((null? tup) 0)
	  (else
		(plus (car tup) (addtup (cdr tup)))))))

(define multiple
  (lambda(x y)
	(cond
	  ((zero? y) 0)
	  (else
		(plus x (multiple x (sub1 y)))))))

(define tup+
  (lambda(tup1 tup2)
	(cond
	  ((and (null? tup1) (null? tup2)) '())
	  (else
		(cons
		  (plus (car tup1) (car tup2))
		  (tup+ (cdr tup1) (cdr tup2)))))))

(define tup++
  (lambda(tup1 tup2)
	(cond
	  ((null? tup1) tup2)
	  ((null? tup2) tup1)
	  (else
		(cons
		  (plus (car tup1) (car tup2))
		  (tup+ (cdr tup1) (cdr tup2)))))))

(define power
  (lambda(x y)
	(cond
	  ((zero? y) 1)
	  (else
		(multiple x (power x (sub1 y)))))))

(define firsts (lambda (list)
  (cond
    ((null? list) '() )
    (else
      (cons (car (car l)) (firsts (cdr l)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
          ((eq? (car lat) old)
            (multisubst new old (cons new (cdr lat))))
          (else
            (cons (car lat) (multisubst new old (cdr lat)))))))))
