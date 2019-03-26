#lang racket

(require "startEval.rkt")

(print
 (startEval
 '(let ((inc
         (lambda (x) (+ x (quote 1)))))
        (inc (quote 5)))
 )
)
(newline)
(print "should be 6")
(newline)

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
(print "should be 3628800")
(newline)

(print
 (startEval
  '(letrec ((fib
            (lambda (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
	   
           (fib 7))
 )
)
(newline)
(print "should be 21")
(newline)

(print
 (startEval
  '(letrec ((intersect
             (lambda (s t) 
               (if (equal? s (quote ()))
                 (quote ())
                 (if (member (car s) t)
                   (cons (car s) (intersect (cdr s) t))
                   (intersect (cdr s) t)
                 )
               )
              ))
             (member
	      (lambda (x s)
                 (if (equal? s (quote ()))
                   (quote #f)
                   (if (equal? x (car s))
                     (quote #t)
                     (member x (cdr s))
                   )
	         )
              )
	     ))
           (intersect (quote (a b c d)) (quote (b c d e f)))
    )
 )
)

(newline)
(print "should be (b c d)")
(newline)

(print
 (startEval '(
              (
                (lambda (x) (lambda (y) (+ x y)))
                1
              )
              2
            )
 )
)
(newline)
(print "should be 3")
(newline)

(print
 (startEval '(let ((+ (lambda (x) (cdr x)))
                   (- '(1 2 3 4 5)))
               (+ -))
 )
)

(newline)
(print "should be (2 3 4 5)")
(newline)


(print
 (startEval (let ([sub1 (lambda (x) (- x 1))]
                  [not (lambda (x) (if x #f #t))])
                  
              
              (letrec ([is-even? (lambda (n)
                                   (if (= n '0)
                                       #t
                                       (is-odd? (sub1 n))))]
                       [is-odd? (lambda (n)
                                  (if (not (= n '0))
                                      (is-even? (sub1 n))
                                      '#f
                                      ))])
                (is-odd? 11)))))

(newline)
(print "should be true")
(newline)
