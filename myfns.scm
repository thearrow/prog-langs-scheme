;JACOB KLINGLER
;klinglej
;klingler.45@osu.edu
;Programming Languages Scheme Lab

;lists L1-L5 defined for testing purposes
(define L1 (quote ()))
(define L2 (quote (1 2 3 4)))
(define L3 (quote (4 5 6 7)))
(define L4 (quote (9 9 9 8 3 1 2 7 7 7 6)))
(define L5 (quote (4)))

;eliminate all items in L2 from L1
(define (elim L1 L2)
	(cond
		((null? L2) L1)
		((null? L1) '())
		(#t (elim (deleteall (car L2) L1) (cdr L2)))
	)
)

;remove all instances of item X from list L
(define (deleteall X L)
	(cond 
		((null? L) '())
		((= X (car L)) (deleteall X (cdr L)))
		(#t (cons (car L) (deleteall X (cdr L))))
	)
)

;remove item X from list L
(define (delete X L)
	(cond 
		((null? L) '())
		((= X (car L)) (cdr L))
		(#t (cons (car L) (delete X (cdr L))))
	)
)

;return sorted list L
(define (sort L)
	(cond
		((null? L) '())
		((null? (cdr L)) L)
		(#t (cons (min L) (sort (delete (min L) L))))
	)
)

;return the minimum element of a list L
(define (min L)
	(cond
		((null? L) '())
		((null? (cdr L)) (car L))
		(#t (smaller (car L) (min (cdr L))))
	)
)

;return the smaller of two elements
(define (smaller X Y)
	(cond
		((< X Y) X)
		(#t Y)
	)
)

;remove elements in L2 from L1, sort remaining elements
(define (eliminateNsort L1 L2)
	(sort (elim L1 L2))
)	