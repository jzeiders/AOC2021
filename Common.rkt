#lang racket

(require racket/list)

(define (sum xs) (foldr + 0 xs))
(define (product xs) (foldr * 1 xs))
(define (zip-with f . args)
	(if (ormap empty? args)
			'()
			(cons (f (map car args)) (apply zip-with (cons f (map cdr args))))))



(provide (all-defined-out))
