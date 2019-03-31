#lang racket
(require racket/trace)

;our environment has this form:
;(name value EnvironmentAtDeclaration Type)
;here, type indicates whether this is a normal let expression or a letrec expression (% means normal function, $ means it is a letrec expression)

;the start eval function
;simply calls our modified startEvalList function that contains a parameter for our current environment
(define (startEval prog)
  (startEvalList prog `()))

;our main start eval function
;takes a program and a current environment and returns the result of evaluating that program with that current environment
;prog -- the program that we wish to evaluate
;environment -- the current environment at the time of calling this function
(define (startEvalList prog environment)
  (if (empty? prog)
      prog
      (if (linSearch environment (if (pair? prog) (car prog) prog))
          (searchEnvironment prog environment)
          (if (not (pair? prog))
              prog
              (if (and (empty? (cdr prog)) (pair? (car prog)))
                  (startEvalList (car prog) environment)
                  (if (equal? (car prog) `quote)
                      (car (cdr prog))
                      (if (arithmaticExpr? (car prog))
                          (arithmaticExpr prog environment)
                          (if (relationalExpr? (car prog))
                              (relationalExpr prog environment)
                              (if (listOperator? (car prog))
                                  (listOperation prog environment)
                                  (if (equal? (car prog) `if)
                                      (ifEval prog environment)
                                      (if (equal? (car prog) `let)
                                          (letEval prog environment)
                                          (if (equal? (car prog) `letrec)
                                              (letrecEval prog environment)
                                              (if (equal? (car (flatten prog)) `lambda)
                                                  (lambdaEval prog environment)
                                                  prog)  ;should we just return the prog if all of the above cases fail?
                                              ))))))))))))


(define (searchEnvironment prog environment)
 (if (pair? prog)
      (if (not (empty? (cdr prog)))
          (if (isLetrecExpression environment (car prog))
              (startEvalList (append (list (linSearchVal environment (car prog))) (map (lambda (x y) (startEvalList x y)) (cdr prog) (make-list (length (cdr prog)) environment))) (list (getEnvironmentWithSymbol environment (car prog)) (linSearchEnvironment environment (car prog))))
              (startEvalList (append (list (linSearchVal environment (car prog))) (map (lambda (x y) (startEvalList x y)) (cdr prog) (make-list (length (cdr prog)) environment))) (linSearchEnvironment environment (car prog))))
          (startEvalList (list (linSearchVal environment (car prog))) (linSearchEnvironment environment (car prog))));perform a start eval here in the case where a parameter is defined in terms of another parameter
      (startEvalList (linSearchVal environment prog) (linSearchEnvironment environment prog))))
  

;function that evaluates a given arithmatic expression, depending on what the car of the parameter Expr is (i.e. +, -, *, or /)
;Expr -- The arithmatic expression to be evaluated
;environment -- our current environment
(define (arithmaticExpr Expr environment)
  (case (car Expr)
    (`+ (+ (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    (`- (- (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    (`* (* (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    (`/ (/ (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
    ))

;function that evaluates a given relational expression, and returns true or false depending on its value
;Expr -- The relational expression to evaluate
;environment -- our current environment
(define (relationalExpr Expr environment)
  (case (car Expr)
    (`equal? (equal? (startEvalList (car (cdr Expr)) environment) (startEvalList (car (cdr (cdr Expr))) environment)))
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
    (`pair? (pair? (startEvalList (car (cdr Expr)) environment)))
    ))

;function that evaluates an if expression, and depending on the result of its conditional branches to the appropriate part of the list
;Expr -- The expression that we wish to evaluate for the if statement
;environment -- our current environment
(define (ifEval Expr environment)
  (if (startEvalList (car (cdr Expr)) environment)
      (startEvalList (car (cdr (cdr Expr))) environment)
      (startEvalList (car (cdr (cdr (cdr Expr)))) environment)))

;function that evaluates and modifies the current environment for a lambda expression
;expr -- our lambda expression
;environment -- our current environment; note that this is modified when we call startEvalList, as we append our new environment to the front of the list
(define (lambdaEval expr environment)
  (startEvalList (car (cdr (cdr (car expr)))) (list (modifyEnvironment (car (cdr (car expr))) (cdr expr) environment) environment)))

(define (letEval expr environment)
  (startEvalList (car (cdr (cdr expr))) (cons (modifyLetEnvironment (car (cdr expr)) environment) environment)))

(define (letrecEval expr environment)
  (startEvalList (car (cdr (cdr expr))) (append (modifyLetrecEnvironment (car (cdr expr)) (modifyLetrecEnvironment (car (cdr expr)) environment)) environment)))
   
;helper function that returns a modified list for an updated environment given parameters of a lambda function and the arguments to that function
;params -- our variable names for our new environment
;arguments -- the values of the variables
;environment -- current environment that will be modified
(define (modifyEnvironment params arguments environment)
  (if (= (length params) 1)
      (list (list (car params) (startEvalList (if (pair? arguments) (car arguments) arguments) environment) environment `%) environment)
      (list (list (car params) (startEvalList (car arguments) environment) environment `%) (modifyEnvironment (cdr params) (cdr arguments) environment) environment)))

(define (modifyLetEnvironment params environment)
  (if (= (length params) 1)
      (list (car (car params)) (car (cdr (car params))) environment `%)
      (list (list (car (car params)) (car (cdr (car params))) environment `%) (modifyLetEnvironment (cdr params) environment))))

(define (modifyLetrecEnvironment params environment)
  (if (= (length params) 1)
      (list (car (car params)) (car (cdr (car params))) environment `$) ;(list (car (car params)) (car (cdr (car params))) environment) `$)
      (list (list (car (car params)) (car (cdr (car params))) environment `$) (modifyLetrecEnvironment (cdr params) environment))))

;determines if the parameter expr is one of the four arithmatic operators
(define (arithmaticExpr? expr)
  (or (or (or (equal? expr `+) (equal? expr `-)) (equal? expr `*)) (equal? expr `/)))

;determines if the paramter expr is one of the six relational operators
(define (relationalExpr? expr)
  (or (or (or (or (or (equal? expr `=) (equal? expr `<=)) (equal? expr `<)) (equal? expr `>=)) (equal? expr `>)) (equal? expr `equal?)))

;determine if the parameter is one of the four list operators
(define (listOperator? expr)
  (or (or (or (equal? expr `cons) (equal? expr `car)) (equal? expr `cdr)) (equal? expr `pair?)))

;function that performs a linear search through an environment looking for the symbol specified and returns true if that symbol is found, false otherwise
;environment -- the environment we want to search for symbol in
;symbol -- the variable name/lexeme that we want to find in the environment
(define (linSearch environment symbol)
  (if (empty? environment)
      false
      (if (equal? (car environment) symbol)
          true
          (if (pair? (car environment)) (or (linSearch (car environment) symbol)  (linSearch (cdr environment) symbol)) false))))

;function that returns the value attached to a particular symbol in our current environment
;this function makes the assumption that the symbol we are looking for exists in our list, so the function linSearch should be called and return true before this one is called
;environment -- the environment we want to search for symbol in
;symbol -- the variable/lexeme that we want to search for its attached value
(define (linSearchVal environment symbol)
  (if (equal? (car environment) symbol)
      (car (cdr environment))
      (if (linSearch (car environment) symbol)
          (linSearchVal (car environment) symbol)
          (linSearchVal (cdr environment) symbol))))


(define (linSearchEnvironment environment symbol)
  (if (equal? (car environment) symbol)
      (car (cdr (cdr environment)))
      (if (linSearch (car environment) symbol)
          (linSearchEnvironment (car environment) symbol)
          (linSearchEnvironment (cdr environment) symbol))))

(define (isLetrecExpression environment symbol)
  (if (equal? (car environment) symbol)
      (equal? (car (cdr (cdr (cdr environment)))) `$)
      (if (linSearch (car environment) symbol)
          (isLetrecExpression (car environment) symbol)
          (isLetrecExpression (cdr environment) symbol))))

(define (getEnvironmentWithSymbol environment symbol)
    (if (equal? (car environment) symbol)
      environment
      (if (linSearch (car environment) symbol)
          (getEnvironmentWithSymbol (car environment) symbol)
          (getEnvironmentWithSymbol (cdr environment) symbol))))






;SAMPLE PROGRAMS

;tests for multiply defined names in lambda and let
(define letExprTest
  `(let ((y 5))
     (let ((f (lambda (x) (* x y))))
       (let ((y 3))
         (f y)))))

;finds the summation of i from one to n
(define sum
  `(letrec ((summation (lambda (x)
                         (if (<= x 0)
                             0
                             (+ x (summation (- x 1)))))))
     (summation 50)))

;raises a to the power b
(define power
  `(letrec ((pow (lambda (a b)
                   (if (<= b 0)
                       (quote 1)
                       (* a (pow a (- b 1)))))))
     (pow (quote 2) (quote 20))))

;performs the set difference of two lists
(define setdifference
  `(letrec ((setdifference
             (lambda (a b)
               (if (equal? a (quote ()))
                   (quote ())
                   (if (member (car a) b)
                       (setdifference (cdr a) b)
                       (cons (car a) (setdifference (cdr a) b))))))
            (member
             (lambda (x s)
               (if (equal? s (quote ()))
                   (quote #f)
                   (if (equal? x (car s))
                       (quote #t)
                       (member x (cdr s)))))))
     (setdifference (quote (a b c d e)) (quote (c d a e f g)))))

(define isodd `(let ([sub1 (lambda (x) (- x 1))]
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
                (is-odd? 11))))