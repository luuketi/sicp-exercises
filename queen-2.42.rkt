#lang racket


(define (print-board board)

  (define (printrow row)
  (when (not (null? row))
      (begin
        (display (car row))
        (display " ")
        (printrow (cdr row)))))
  
  (if (null? board)
      (display "\n")
    (begin
      (printrow (car board))
      (display "\n")
      (print-board (cdr board)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ 1 low) high))))


(define (put-q-in-position pos size)
  (if (= 0 size)
      '()
      (if (= 1 pos)
          (cons 'Q (put-q-in-position (- pos 1) (- size 1)))
          (cons '_ (put-q-in-position (- pos 1) (- size 1))))))


(define (get-value position row)
      (define (get-pos-rec acc pos row)
        (cond [(null? row) '()]
              [(= acc pos) (car row)]
              [else (get-pos-rec (+ 1 acc) pos (cdr row))]))
      (get-pos-rec 1 position row))


(define (safe? k board)
      
  (define (generic-check-queen position function-to-move-position-for-the-next-row board)

    (define (next-positions-are-empty? position board)
      (cond [(null? board) true]
            [(equal? 'Q (get-value position (car board))) false]
            [else (next-positions-are-empty? (function-to-move-position-for-the-next-row position) (cdr board))]))
    
    (cond [(null? board) true]
          [(equal? 'Q (get-value position (car board))) (next-positions-are-empty? (function-to-move-position-for-the-next-row position) (cdr board))]
          [else (generic-check-queen position function-to-move-position-for-the-next-row (cdr board))]))

  (define (move-diagonal-forward position)  (+ position 1))
  (define (move-diagonal-backward position) (- position 1))
  (define (keep-vertical-position position)    position)
 
  (define (check-queen-is-unique start k board)      
    (cond [(> start k) true] 
          [(equal? false (and (generic-check-queen start move-diagonal-forward board)
                              (generic-check-queen start move-diagonal-backward board)
                              (generic-check-queen start keep-vertical-position board))) false]
          [else (check-queen-is-unique (+ 1 start) k board)]))
  

  (check-queen-is-unique 1 k board))
    

(define empty-board '())


(define (queens board-size)

  (define (adjoin-position new-row k rest-of-queens)
    (cons (put-q-in-position new-row board-size) rest-of-queens))
  
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens) (map (lambda (new-row)
                                          (adjoin-position new-row k rest-of-queens))
                                          (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  
  (queen-cols board-size))

(define size 9)

(length (map print-board (queens size)))
