#lang racket

(require "../../e0.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (point-distance 0 0 3 4))))
