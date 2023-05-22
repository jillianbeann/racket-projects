#lang racket

(require "../../p1.rkt")

(with-output-to-file "output"
                     (lambda ()
                       (print (make-move '(E E E
                                           E E E 
                                           E E E) 0 0 'X))))