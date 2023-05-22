#lang racket

(require "../../p1.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (make-move '(X E O
                                           O X E 
                                           X O E) 2 2 'X))))