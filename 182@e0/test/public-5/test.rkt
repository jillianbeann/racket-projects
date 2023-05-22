#lang racket

(require "../../e0.rkt")


(with-output-to-file "output"
                     (lambda ()
                       (print (point-distance 8 142 242 18))))
