#lang racket

(require "../../p1.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (winner? '(X O O
                                         X E O 
                                         O X X)))))