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

(define division
  (lambda (n m)
	(cond
	  ((< n m) 0)
	  (else
		(add1 ( division (minus n m) m))))))

(define length
  (lambda (lat)
	(cond
	  ((null? lat) 0)
	  (else
		(add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
	(cond
	  ((zero? (sub1 n)) (car lat))
	  (else
		(pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
	(cond
	  ((zero? (sub1 n)) (cdr lat))
	  (else
		(cons (car lat)
			  (rempick (sub1 n) (cdr lat)))))))

(define no-num
  (lambda (lat)
	(cond
	  ((null? lat) '())
	  (else
		(cond
		  ((number? (car lat)) (no-num (cdr lat)))
		  (else
			(cons
			  (car lat)
			  (no-num (cdr lat)))))))))

(define all-num
  (lambda (lat)
	(cond
	  ((null? lat) '())
	  (else
		(cond
		  ((number? (car lat)) (cons (car lat) (all-num (cdr lat))))
		  (else
			(all-num (cdr lat))))))))

(define eqan?
  (lambda (a1 a2)
	(cond
	  ((and (number? a1) (number? a2))
	   (= a1 a2))
	  ((or (number? a1) (number? a2))
	   #f)
	  (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
	(cond
	  ((null? lat) 0)
	  (else
		(cond
		  ((eq? a (car lat))
		   (add1 (occur a (cdr lat))))
		  (else
			(occur a (cdr lat))))))))

(define one?
  (lambda (n)
	(cond
	  (else ( zero? (sub1 n))))))
