#lang racket

(require "startEval.rkt")

(print 
 (startEval
 '(letrec ((fact
           (lambda (x)
	     (if (= x 0) (quote 1)
		(* x (fact (- x 1)))))))
	  (fact 10))
 )
)

(newline)

(print
 (startEval
  '(let ((y 10))
     (let ((f (lambda (x) (+ x y))))
       (let ((y 100))
         (f 2))))))

(newline)
