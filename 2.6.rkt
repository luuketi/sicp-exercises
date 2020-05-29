#lang racket

; In case representing pairs as procedures wasn't mind-boggling enough, consider that, in a language that can manipulate procedures, we can get by without numbers (at least insofar as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as

(define zero
  (λ(f) (λ(x) x)))

(define (add-1 n)
  (λ(f) (λ(x) (f ((n f) x)))))

(define one (add-1 zero))
(define two (add-1 one))

(define (count value)
  ((value (λ(a) (+ 1 a))) 0))

(count zero)
(count one)
(count two)
(count (add-1 (add-1 (add-1 (add-1 two)))))


; This representation is known as Church numerals, after its inventor, Alonzo Church, the logician who invented the λ calculus.

; Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate (add-1 zero)). Give a direct definition of the addition procedure + (not in terms of repeated application of add-1). 

(define one-directly
  (λ(f) (λ(x) (f x))))
    
(count one-directly)

(define two-directly
  (λ(f) (λ(x) (f (f x)))))

(count two-directly)

(define (add a b) 
  (λ(f) (λ(x) ((a f) ((b f) x)))))

(count (add one-directly two-directly))
(count (add zero one-directly))
(count (add zero zero))
(count (add (add-1 (add-1 (add-1 (add-1 two-directly)))) (add-1 (add-1 (add-1 (add-1 two-directly))))))

