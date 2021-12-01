#lang racket

(require racket/list)

(define (id x) x)

(define (zip xs ys)
	(if (or (empty? xs) (empty? ys))
			'()
			(cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

(define (sum xs) (foldr + 0 xs))
(define (zip-with f . args)
	(if (ormap empty? args)
			'()
			(cons (f (map car args)) (apply zip-with (cons f (map cdr args))))))

(define input (map string->number (file->lines "input1.txt")))

(define (increasing-sums xs)
	(define next (drop xs 1))
	(length (filter id (map (lambda (pair) (> (cdr pair) (car pair))) (zip xs next)))))

(define (part-1) (increasing-sums input))

(define (part-2)
	(define sliding-window (zip-with sum input (drop input 1) (drop input 2)))
	(increasing-sums sliding-window)
)
