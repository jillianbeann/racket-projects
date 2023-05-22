#lang racket

(require "../../p1.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (winner? '(X X X 
                                         E E E 
                                         E O O)))))