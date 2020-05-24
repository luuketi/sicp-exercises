#lang racket

; Exercise 2.4
; Here is an alternative procedural representation of pairs. For this representation, verify that (car (cons x y)) yields x for any objects x and y.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; What is the corresponding definition of cdr? (Hint: To verify that this works, make use of the substitution model of Section 1.1.5)

(define a (cons 1 2))
(car a)
; (car z)
; (z (λ (p q) p))
; ((cons 1 2) (λ (p q) p))
; ((λ (m) (m 1 2)) (λ (p q) p))
; ((λ (p q) p) 1 2)
; (1)


(define (cdr z)
  (z (lambda (p q) q)))

(cdr a)
