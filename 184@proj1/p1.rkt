#lang racket

;;; Project 0 Tic-tac-toe with Racket
;;; 
;;; Please immediately read README.md

(provide board?
         next-player
          valid-move?
          make-move
          winner?
          calculate-next-move)

;; 
;; Useful utility functions
;;

; Returns the number of elements in l for which the predicate f
; evaluates to #t. For example:
;
;    (count (lambda (x) (> x 0)) '(-5 0 1 -3 3 4)) => 3
;    (count (lambda (x) (= x 0)) '(-5 0 1 -3 3 4)) => 1
(define (count f l)
  (cond [(empty? l) 0]
        [(f (car l)) (add1 (count f (cdr l)))]
        [else (count f (cdr l))]))

;; 
;; Your solution begins here
;; 

; Check whether a list is a valid board
(define (board? lst)
  (if (empty? lst)
      #f
      (if (and
           (not (equal?
                (* (integer-sqrt (count (lambda (x) (symbol? x)) lst))
                   (integer-sqrt (count (lambda (x) (symbol? x)) lst)))
                (count (lambda (x) (symbol? x)) lst)))
           (not (= (count (lambda (x) (symbol? x)) lst) 2)))
          #f
          (if (not (equal?
                    (+ (count (lambda (x) (equal? x 'E)) lst)
                       (count (lambda (x) (equal? x 'X)) lst)
                       (count (lambda (x) (equal? x 'O)) lst))
                    (count (lambda (x) (symbol? x)) lst)))
              #f
              (if (<
                     (- (count (lambda (x) (equal? x 'X)) lst)
                        (count (lambda (x) (equal? x 'O)) lst)) 0)
                  #f
                  (if (>
                            (- (count (lambda (x) (equal? x 'X)) lst)
                               (count (lambda (x) (equal? x 'O)) lst)) 1)
                  #f
                  (if (<
                       (- (count (lambda (x) (equal? x 'X)) lst)
                          (count (lambda (x) (equal? x 'O)) lst))
                       0)
                      #f
                      #t)))))))

;;; From the board, calculate who is making a move this turn
(define (next-player board)
  (if (=
        (- (count (lambda (x) (equal? x 'X)) board)
           (count (lambda (x) (equal? x 'O)) board)) 1)
      'O
      'X))

;;; If player ('X or 'O) want to make a move, check whether it's this
;;; player's turn and the position on the board is empty ('E)
(define (valid-move? board row col player)
  (if (not (board? board))
      #f
      (if (not (and (number? row) (number? col)))
          #f
          (if (not (equal? player (next-player board)))
              #f
              (if (and (= row col 0) (equal? (first board) 'E))
                  #t
                  (if (>
                       (+ (* (sqrt (length board)) row) col)
                       (-(length board) 1))
                       #f       
                       (if (equal?
                            (list-ref board
                                      (+ (* (sqrt (length board)) row) col))
                            'E)
                           #t
                      #f)))))))
                       
                       

;;; To make a move, replace the position at row col to player ('X or 'O)
(define (make-move board row col player)
  (if (not (board? board))
      board
      (if (negative-integer? row)
          board
          (if (negative-integer? col)
              board
              (append (take board (+ (* (sqrt (length board)) row) col))
                      (list (next-player board))
                      (list-tail board (+ (* (sqrt (length board)) row) col 1)))))))

;;; To determine whether there is a winner?
(define (get-diags-left board dim location)
    (cond
    [(equal? location 0) '()]
    [(append (list (list-tail(take board dim) (- dim 1)))
             (get-diags-left (append (take board dim) '(E E))
                              dim
                              (- location 1)))]))
  
(define (get-diags-right board dim location)
  (cond
    [(equal? location 0) '()]
    [(append (list (first board))
             (get-diags-right (append (list-tail board (+ dim 1)) '(E E))
                              dim
                              (- location 1)))]))
  
(define (get-rows board dim)
  (cond
    [(empty? board) '()]
    [(append (list (take board dim)) (get-rows (list-tail board dim) dim))]))
    
(define (get-colf board dim)
  (cond
    [(empty? board) '()]
    [(append (list (car board)) (get-colf (list-tail board dim) dim))]))
           
(define (get-all-col board dim times)
  (cond
    [(equal? times 0) '()]
    [(append (list (get-colf board dim)) (get-all-col (append (rest board) '(E)) dim (- times 1)))]))

(define (all-player lst player)
  (andmap (lambda (x) (equal? x player)) lst))

(define (all-player-nest lst player)
  (cond
    [(empty? lst) '()]
    [(cons (all-player (car lst) player) (all-player-nest (cdr lst) player))]))
    
(define (check-true lst)
  (if (empty? lst)
      #f
      (if (equal? (car lst) #t)
          #t
          (check-true (cdr lst)))))
    
(define (winner? board)
  (define dimension (sqrt (length board)))
  (define rows (get-rows board dimension))
  (define cols (get-all-col board dimension dimension))
  (define diagsright (get-diags-right board dimension dimension))
  (define diagsleft (get-diags-right board dimension dimension))
  ;; code that checks whether any row, col, or diag is all 'X or 'O
  (cond
    [(all-player diagsright 'X) 'X]
    [(all-player diagsleft 'O) 'O]
    [(all-player diagsright 'O) 'O]
    [(all-player diagsleft 'X) 'X]
    [(check-true (all-player-nest rows 'X)) 'X]
    [(check-true (all-player-nest rows 'O)) 'O]
    [(check-true (all-player-nest cols 'O)) 'O]
    [(check-true (all-player-nest cols 'X)) 'X]
    [else #f])
  )
      

;;; The board is the list containing E O X 
;;; Player will always be 'O
;;; returns a pair of x and y
(define (calculate-next-move board player)
  'todo)

