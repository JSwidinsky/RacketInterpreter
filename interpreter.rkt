#lang racket

(define (startEval prog)
  (startEvalList prog `()))

(define (startEvalList prog environment)
  (if (linSearch environment (if (pair? prog) (car prog) prog))
      (linSearchVal environment (if (pair? prog) (car prog) prog))
      (if (not (pair? prog))
          prog
          (if (empty? (cdr prog))
              prog   ;this used to be (cdr prog); can I remember why? (changed it so that car would work with input `(6))
              (if (equal? (car prog) `quote)
                  (quote (cdr prog))
                  (if (arithmaticExpr? (car prog))
                      (arithmaticExpr prog environment)
                      (if (relationalExpr? (car prog))
                          (relationalExpr prog environment)
                          (if (listOperator? (car prog))
                              (listOperation prog environment)
                              (if (equal? (car prog) `if)
                                  (ifEval prog environment)
                                  (if (equal? (car (car prog)) `lambda)
                                      (lambdaEval prog environment)
                                      prog)   ;should we just return the prog if all of the above cases fail?
                              )))))))))


;function that evaluates a given arithmatic expression, depending on what the car of the parameter Expr is (i.e. +, -, *, or /)
;Expr -- The arithmatic expression to be evaluated
(define (arithmaticExpr Expr environment)
  (case (car Expr)
    (`+ (+ (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    (`- (- (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    (`* (* (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    (`/ (/ (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    ))

;function that evaluates a given relational expression, and returns true or false depending on its value
;Expr -- The relational expression to evaluate
(define (relationalExpr Expr environment)
  (case (car Expr)
    (`equal? (equal? (car (cdr Expr)) (car (cdr (cdr Expr)))))
    (`= (= (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    (`<= (<= (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    (`< (< (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    (`>= (>= (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    (`> (> (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    ))

;this might not work yet...
(define (listOperation Expr environment)
  (case (car Expr)
    (`car (car (startEvalList (car (cdr Expr)) environment)))
    (`cdr (cdr (startEvalList (car (cdr Expr)) environment)))
    (`cons (cons (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    (`pair? (pair? (car (cdr Expr))))
    ))

;function that evaluates an if expression, and depending on the result of its conditional branches to the appropriate part of the list
;Expr -- The expression that we wish to evaluate for the if statement
(define (ifEval Expr environment)
  (if (startEvalList (car (cdr Expr)) environment)
      (startEvalList (car (cdr (cdr Expr))) environment)
      (startEvalList (car (cdr (cdr (cdr Expr)))) environment)))

;function that evaluates and modifies the current environment for a lambda expression
(define (lambdaEval expr environment)
  (startEvalList (car (cdr (cdr (car expr)))) (cons (modifyEnvironment (car (cdr (car expr))) (cdr expr) environment) environment)))

;helper function that returns a modified list for an updated environment given parameters of a lambda function and the arguments to that function
(define (modifyEnvironment params arguments environment)
  (if (= (length params) 1)
      (cons (flatten (cons params (startEvalList (if(pair? arguments) (car arguments) (arguments)) environment))) '())
      (cons (flatten (cons (car params) (startEvalList (car arguments) environment))) (modifyEnvironment (cdr params) (cdr arguments) environment))))

;determines if the parameter expr is one of the four arithmatic operators
(define (arithmaticExpr? expr)
  (or (or (or (equal? expr `+) (equal? expr `-)) (equal? expr `*)) (equal? expr `/)))

;determines if the paramter expr is one of the six relational operators
(define (relationalExpr? expr)
  (or (or (or (or (or (equal? expr `=) (equal? expr `<=)) (equal? expr `<)) (equal? expr `>=)) (equal? expr `>)) (equal? expr `equal?)))

;determine if the parameter is one of the four list operators
(define (listOperator? expr)
  (or (or (or (equal? expr `cons) (equal? expr `car)) (equal? expr `cdr)) (equal? expr `pair?)))

;function that performs a linear search through an environment looking for the symbol specified
(define (linSearch environment symbol)
  (if (empty? environment)
      false
      (if (equal? (car environment) symbol)
          true
          (if (if (pair? (car environment)) (linSearch (car environment) symbol) false)
              true
              (linSearch (cdr environment) symbol)))))

;function that returns the value attached to a particular symbol in our current environment
;this function makes the assumption that the symbol we are looking for does exist in our list, so the function linSearch should be called and return true before this one is called
(define (linSearchVal environment symbol)
  (if (equal? (car environment) symbol)
      (car (cdr environment))
      (if (linSearch (car environment) symbol)
          (linSearchVal (car environment) symbol)
          (linSearchVal (cdr environment) symbol))))

;put any sample test programs here
(define sampleProg `(cons 6 (cdr (5 8))))  ;this may not work quite right yet?
(define s `(car (6)))
(define st `(if (< 6 (+ 4 1))
                (equal? (car (4 5)) (cdr (5 4)))
                (+ (* 6 7) (- (/ 10 5) 1))))

(define lam `((lambda (x y) (+ ((lambda (a) (* a a)) 2) y)) (+ 3 4) (- 4 5)))
(define lin `(((x 4) (y 5) (z 6)) (a 5) (b 6)))