(import (rnrs base)
	(letrec-values)
        (srfi :64)
        )

(test-group "letrec*-values"
  (test-equal "empty bindings"
    #t
    (letrec*-values () #t))

  (test-equal "nonrecursive, independent bindings 1"
    4
    (letrec*-values (((a) 4)) a))

  (test-equal "nonrecursive, independent bindings 2"
    6
    (letrec*-values (((a) 4) ((b) 2)) (+ a b)))

  (test-equal "nonrecursive, independent bindings 3"
    17
    (letrec*-values (((a) 4)
                     ((b c) (values 2 11)))
      (+ a b c)))

  (test-equal "nonrecursive, independent bindings 4"
    25
    (letrec*-values (((a) 4)
                     ((b c) (values 2 11))
		     ((d . rest) (values 3 3 2)))
      (apply + a b c d rest)))

  (test-equal "nonrecursive, independent bindings 5"
    25
    (letrec*-values (((a) 4)
                     ((b c) (values 2 11))
		     (d (values 3 3 2)))
      (apply + a b c d)))

  (test-equal "nonrecursive, independent bindings 6"
    25
    (letrec*-values (((a) 4)
                     ((b c) (values 2 11))
                     (d (values 3 3 2))
                     (() (values)))
      (apply + a b c d)))

  (test-equal "nonrecursive, sequential bindings 1"
    17
    (letrec*-values (((a) 10)
		     ((b) (- a 3)))
      (+ a b)))

  (test-equal "nonrecursive, sequential bindings 2"
    17
    (letrec*-values (((a . rest) (values 1 2 3))
		     ((b c) (values (+ a 9) (apply * rest))))
      (+ a b c)))

  (test-equal "nonrecursive, sequential bindings 3"
    17
    (let ((a 3))
      (letrec*-values (((a) 10)
  		       ((b) (- a 3)))
        (+ a b))))

  (test-equal "recursive, independent bindings 1"
    '(2 4 6)
    (letrec*-values (((mapp)
		      (lambda (f list)
			(if (null? list)
			    '()
			    (cons (f (car list))
				  (mapp f (cdr list)))))))
      (mapp (lambda (x) (* x 2)) '(1 2 3))))

  (test-equal "recursive, independent bindings 2"
    '(2 4 6)
    (letrec*-values (((mapp . ints)
                      (values
	 	       (lambda (f list)
		 	 (if (null? list)
			     '()
			     (cons (f (car list))
				   (mapp f (cdr list)))))
		       1
		       2
		       3)))
      (mapp (lambda (x) (* x 2)) ints)))

  (test-equal "recursive, sequential bindings 3"
    '(2 4 6)
    (letrec*-values (((mapp)
		      (lambda (f list)
			(if (null? list)
			    '()
			    (cons (f (car list))
				  (mapp f (cdr list))))))
		     ((doubles) (mapp (lambda (n) (* n 2)) '(1 2 3))))
      doubles))

  (test-equal "mutually-recursive bindings 1"
    #t
    (letrec*-values (((even?)
		      (lambda (n)
			(if (zero? n)
			    #t
			    (odd? (- n 1)))))
		     ((odd?)
		      (lambda (n)
			(if (zero? n)
			    #f
			    (even? (- n 1))))))
      (odd? 9)))

  (test-equal "mutually-recursive bindings 2"
    #t
    (letrec*-values (((even? odd?)
		      (values
		       (lambda (n)
			 (if (zero? n)
			     #t
			     (odd? (- n 1))))
		       (lambda (n)
			 (if (zero? n)
			     #f
			     (even? (- n 1)))))))
      (even? 10)))
  )


(test-group "letrec-values"
  (test-equal "empty bindings"
    #t
    (letrec-values () #t))

  (test-equal "nonrecursive bindings 1"
    4
    (letrec-values (((a) 4)) a))

  (test-equal "nonrecursive bindings 2"
    6
    (letrec-values (((a) 4) ((b) 2)) (+ a b)))

  (test-equal "nonrecursive bindings 3"
    17
    (letrec-values (((a) 4)
                     ((b c) (values 2 11)))
      (+ a b c)))

  (test-equal "nonrecursive bindings 4"
    25
    (letrec-values (((a) 4)
                    ((b c) (values 2 11))
                    (d (values 3 3 2))
                    (() (values)))
      (apply + a b c d)))

  (test-equal "nonrecursive bindings 4"
    25
    (letrec-values (((a) 4)
                     ((b c) (values 2 11))
		     ((d . rest) (values 3 3 2)))
      (apply + a b c d rest)))

  (test-equal "recursive bindings 1"
    '(2 4 6)
    (letrec-values (((mapp)
		      (lambda (f list)
			(if (null? list)
			    '()
			    (cons (f (car list))
				  (mapp f (cdr list)))))))
      (mapp (lambda (x) (* x 2)) '(1 2 3))))

  (test-equal "recursive bindings 2"
    '(2 4 6)
    (letrec-values (((mapp . ints)
                     (values
		      (lambda (f list)
			(if (null? list)
			    '()
			    (cons (f (car list))
				  (mapp f (cdr list)))))
		      1
		      2
		      3)))
      (mapp (lambda (x) (* x 2)) ints)))

  (test-equal "mutually-recursive bindings 1"
    #t
    (letrec-values (((even?)
		      (lambda (n)
			(if (zero? n)
			    #t
			    (odd? (- n 1)))))
		     ((odd?)
		      (lambda (n)
			(if (zero? n)
			    #f
			    (even? (- n 1))))))
      (odd? 9)))

  (test-equal "mutually-recursive bindings 2"
    #t
    (letrec-values (((even? odd?)
		      (values
		       (lambda (n)
			 (if (zero? n)
			     #t
			     (odd? (- n 1))))
		       (lambda (n)
			 (if (zero? n)
			     #f
			     (even? (- n 1)))))))
      (even? 10)))
  )
