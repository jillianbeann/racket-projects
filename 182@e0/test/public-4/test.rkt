#lang racket

(require "../../e0.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (point-distance 1 65 26 57))))
