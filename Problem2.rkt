#lang racket

(require "common.rkt")
(require racket/list)

(define input (file->lines "input2.txt"))


;TODO: How to make that appliation cleaner?
(define (sum-vectors . vecs) (apply zip-with (cons sum vecs)))
(define (parse-input x)
	(let* ((split (string-split x " "))
				 (direction (car split))
				 (value (string->number (cadr split))))
		(cond
			((equal? direction "forward") (list value 0))
			((equal? direction "down") (list 0 value))
			((equal? direction "up") (list 0 (- value)))
		)
))

(define (make-move move pos)

	(list (+ (car pos) (car move)) (+ (cadr pos) (* (car move) (caddr pos))) (+ (cadr move) (caddr pos)))
)

(define (sol-1 xs)
	(product (foldr sum-vectors '(0 0) (map parse-input xs)))
	)

	(define (sol-2 xs)
		(foldl make-move '(0 0 0) (map parse-input xs))
	)
