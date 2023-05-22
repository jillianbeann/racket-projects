#lang racket

;; Exercise 0: complete two simple functions

(provide implies-value
         point-distance)


; Compute the truth value of the proposition "x --> y" where x and y are booleans
(define (implies-value x y)
  (define (implies-value x y)
  (if (equal? (and x y) #f)
      (if (equal? x #f)
          #t
          (if (equal? y #t)
              #t
              #f))
      #t)))


; Compute the distance between two (x,y) pairs of integers
(define (point-distance x0 y0 x1 y1)
  (sqrt (+ (* (- x1 x0) (- x1 x0)) (* (- y1 y0) (- y1 y0)))))
