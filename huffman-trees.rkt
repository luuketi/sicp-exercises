#lang racket
;(require rnrs)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

﻿(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

﻿(define (make-code-tree left right)
   (list left
         right
         (append (symbols left) (symbols right))
         (+ (weight left) (weight right))))

﻿(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

﻿(define (decode bits tree)

   (define (decode-1 bits current-branch)
     (if (null? bits)
         '()
         (let ((next-branch
                (choose-branch (car bits) current-branch)))
           (if (leaf? next-branch)
               (cons (symbol-leaf next-branch)
                     (decode-1 (cdr bits) tree))
               (decode-1 (cdr bits) next-branch)))))

   (decode-1 bits tree))

﻿(define (choose-branch bit branch)
   (cond ((= bit 0) (left-branch branch))
         ((= bit 1) (right-branch branch))
         (else (error "bad bit: CHOOSE-BRANCH" bit))))


﻿(define sample-tree
   (make-code-tree (make-leaf 'A 4)
                   (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                     (make-leaf 'D 1)
                     (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)


(define (encode-symbol symbol tree)

  (define (left-branch-is-leaf-and-equals-symbol tree symbol)
    (and (leaf? (left-branch tree)) (equal? symbol (symbol-leaf (left-branch tree)))))

  (define (right-branch-is-leaf-and-equals-symbol tree symbol)
    (and (leaf? (right-branch tree)) (equal? symbol (symbol-leaf (right-branch tree)))))
     

  (define (encode-symb symbol tree encoding)
    (cond [(leaf? tree) '()]
          [(left-branch-is-leaf-and-equals-symbol tree symbol)       (append encoding '(0))]
          [(right-branch-is-leaf-and-equals-symbol tree symbol)      (append encoding '(1))]
          [else (append (encode-symb symbol (left-branch tree) (append encoding '(0)))
                        (encode-symb symbol (right-branch tree) (append encoding '(1))))]
          ))
 
  (let ((result (encode-symb symbol tree '())))
    (if (null? result)
        (error "bad symbol ENCODE-SYMBOL" symbol)
        result)))

﻿(define (encode message tree) 
   (if (null? message)
       '()
       (append (encode-symbol (car message) tree)
               (encode (cdr message) tree))))

(encode '(A) sample-tree)
(equal? (encode '(A) sample-tree) '(0))
(encode '(B) sample-tree)
(equal? (encode '(B) sample-tree) '(1 0))
(equal? (encode (decode sample-message sample-tree) sample-tree) '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;(encode '(Z) sample-tree)




﻿(define (adjoin-set x set)
   (cond ((null? set) (list x))
         ((< (weight x) (weight (car set))) (cons x set))
         (else (cons (car set)
                     (adjoin-set x (cdr set))))))

﻿(define (make-leaf-set pairs)
   (if (null? pairs)
       '()
       (let ((pair (car pairs)))
         (adjoin-set (make-leaf (car pair) ; symbol
                                (cadr pair)) ; frequency
                     (make-leaf-set (cdr pairs))))))

(define (delete-leaf-from-set leaf set)
  (let [(leaf-name (symbol-leaf leaf))]
    (cond [(equal? set '()) '()]
        [(equal? (symbol-leaf (car set)) leaf-name) (cdr set)]
        [else (adjoin-set (car set) (delete-leaf-from-set leaf (cdr set)))])))
      
  
(define (successive-merge leaf-set)
  (if (and (not (equal? leaf-set '())) (not (equal? (cdr leaf-set) '())))
      (let* ((first-leaf (car leaf-set))
            (second-leaf (cadr leaf-set))
            (new-subtree (make-code-tree first-leaf second-leaf))
            (leaf-set-without-first-and-second-leaves (delete-leaf-from-set first-leaf (delete-leaf-from-set second-leaf leaf-set))))
        (successive-merge (adjoin-set new-subtree leaf-set-without-first-and-second-leaves)))
      (car leaf-set)))


﻿(define (generate-huffman-tree pairs)
   (successive-merge (make-leaf-set pairs)))

(equal? sample-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))

(define my-tree (generate-huffman-tree '(﻿(A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

(define decoded-song '(﻿GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(define encoded-song (encode decoded-song my-tree))
(length encoded-song)
(length decoded-song)
(equal? decoded-song (decode encoded-song my-tree))