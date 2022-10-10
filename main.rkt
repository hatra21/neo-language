#lang racket
(require "utility.rkt")
(require "parser.rkt")
(require "runner.rkt")
(require "variable_env.rkt")

(define env '((a 1) (b 2) (c 5)))

(define lst '(1 2 3))
(elementAt lst 1)

(define sample-code '(call (function () (ask (bool > a b) (math - a b) (math + a b))) (a)))
(display (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)