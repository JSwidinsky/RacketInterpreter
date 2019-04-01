#lang racket
(require racket/trace)

;NOTES
;Group members: Josh Swidinsky & Adeel Imtiaz

;our environment has this form:
;(name value EnvironmentAtDeclaration Type)
;here, type indicates whether this is a normal let expression or a letrec expression (% means normal function, $ means it is a letrec expression)

;There is a bug involving letrec, where if we have more than one expression in the body, and each of these expressions refers to the other, the program will eventually run out of an environment
;and will simply return a list consisting of an expression. This happens because we do not properly replicate the letrec expressions into a new environment when calling a letrec expression
;This only seems to happen with the isodd sample below. The set difference function works fine
;A possible way to fix this is to have a third parameter detailing all of the letrec expression names in our current scope, that way we can quickly look them up in our environment and append them to
;our new environment



;the start eval function
;simply calls our modified startEvalList function that contains a parameter for our current environment
(define (startEval prog)
  (startEvalList prog `()))


;our main start eval function
;takes a program and a current environment and returns the result of evaluating that program with that current environment
;prog -- the program that we wish to evaluate
;environment -- the current environment at the time of calling this function
(define (startEvalList prog environment)
  (if (empty? prog) ;just return the empty list if our program is nothing but the empty list
      prog
      (if (linSearch environment (if (pair? prog) (car prog) prog)) ;if we find the car of our list in our environment, then find the name and replace it with its value in our environment
          (searchEnvironment prog environment)
          (if (not (pair? prog)) ;if we only have a single atom, just return it
              prog
              (if (and (empty? (cdr prog)) (pair? (car prog))) ;if we have a nested list, and that list doesn't contain a pair, then simply remove one layer of brackets and evaluate the list again
                  (startEvalList (car prog) environment)
                  (if (equal? (car prog) `quote) ;if we are equal to quote, just return the other part of quote
                      (car (cdr prog))
                      (if (arithmaticExpr? (car prog)) ;if the first element is an arithmatic operator, then perform arithmetic on it
                          (arithmaticExpr prog environment)
                          (if (relationalExpr? (car prog)) ;if the first element is a relational operator, then perform the relational operator
                              (relationalExpr prog environment)
                              (if (listOperator? (car prog)) ;if the first element is a list operator, then perform that list operation
                                  (listOperation prog environment)
                                  (if (equal? (car prog) `if) ;if the first element is equal to if, then we must evaluate an if expression
                                      (ifEval prog environment)
                                      (if (equal? (car prog) `let) ;if the first element is equal to let, then we must evaluate it as a let expression
                                          (letEval prog environment)
                                          (if (equal? (car prog) `letrec) ;if the first element is equal to letrec, then we must evaluate it as a letrec expression
                                              (letrecEval prog environment)
                                              (if (equal? (car (flatten prog)) `lambda) ;if the first element is equal to lambda, then we must evaluate it as a lambda expression
                                                  (lambdaEval prog environment)
                                                  prog) ;if we manage to get here, just return the program (all other cases have failed)
                                              ))))))))))))


;function that searches an environment and replaces the car of our list with its value in our environment
;this function does assume that the car of the prog does exist in our environment, so if it is not this function should not be called
;prog -- our program expression
;environment -- the current environment; needed to search through to find the value assocated with the car of prog
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


;function that evaluates a list expression based on what the car of our expression is
;Expr -- our racket program or expression to evaluate
;environment -- the current environment
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


;function that evaluates and modifies the current environment for a let expression
;unlike lambda, this does not evaluate the body of the let expression, but merely stores it in our environment with its name
;expr -- the racket expression/program we are currently interpreting
;environment -- our current environment; this is needed so that we can update our environment accordingly
(define (letEval expr environment)
  (startEvalList (car (cdr (cdr expr))) (cons (modifyLetEnvironment (car (cdr expr)) environment) environment)))


;function that evaluates and modifies the current environment for a letrec expression
;unlike lambda, this does not evaluate the body of the letrec expression, as it is impossible to recursively evaluate the body without knowing the arguments to the letrec expression
;expr -- the racket expression/program
;environment -- our current environment; this is needed so that we may modifiy it accordingly
(define (letrecEval expr environment)
  (startEvalList (car (cdr (cdr expr))) (append (modifyLetrecEnvironment (car (cdr expr)) (append (modifyLetrecEnvironment (car (cdr expr)) environment) environment)) environment)))


;helper function that returns a modified list for an updated environment given parameters of a lambda function and the arguments to that function
;params -- our variable names for our new environment
;arguments -- the values of the variables
;environment -- current environment that will be modified
(define (modifyEnvironment params arguments environment)
  (if (= (length params) 1) ;base case: return the environment for a single element in our environment
      (list (list (car params) (startEvalList (if (pair? arguments) (car arguments) arguments) environment) environment `%) environment)
      (list (list (car params) (startEvalList (car arguments) environment) environment `%) (modifyEnvironment (cdr params) (cdr arguments) environment) environment)))


;helper function that returns a modified environment for a let expression, given the parameters and a current environment
;note that this does not evaluate the body of the let expression, but merely appends its value into our list
;params -- the name-value pairs that are in a let expression
;environment -- our current environment that we are modifying
(define (modifyLetEnvironment params environment)
  (if (= (length params) 1)
      (list (car (car params)) (car (cdr (car params))) environment `%)
      (list (list (car (car params)) (car (cdr (car params))) environment `%) (modifyLetEnvironment (cdr params) environment))))


;helper function that returns a modified environment for a let expression, given the parameters and a current environment
;note that this does not evaluate the body of the letrec expressions for the value pair, as it may be impossible to evaluate this without knowing the arguments
;params -- the name-value pairs that are in our letrec expression
;environment -- our current environment that we are modifying
(define (modifyLetrecEnvironment params environment)
  (if (= (length params) 1)
      (list (car (car params)) (car (cdr (car params))) environment `$)
      (list (list (car (car params)) (car (cdr (car params))) environment `$) (modifyLetrecEnvironment (cdr params) environment))))


;returns true if the car of expression is equal to some arithmatic operator
;expr -- the current expression
(define (arithmaticExpr? expr)
  (or (or (or (equal? expr `+) (equal? expr `-)) (equal? expr `*)) (equal? expr `/)))


;returns true if the car of expression is equal to some relational operator
;expr -- the current expression
(define (relationalExpr? expr)
  (or (or (or (or (or (equal? expr `=) (equal? expr `<=)) (equal? expr `<)) (equal? expr `>=)) (equal? expr `>)) (equal? expr `equal?)))


;retruns true if the car of expression is equal to some list operator
;expr -- the current expression
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


;function that returns an environment associated with a particular symbol in our environment
;this function makes the assumption that the symbol we are looking for exists in our list, so the function linSearch should be called and return true before this one is called
;environment -- the environment we want to search for symbol in
;symbol -- the variable/lexeme that we want to search for its attached value
(define (linSearchEnvironment environment symbol)
  (if (equal? (car environment) symbol)
      (car (cdr (cdr environment)))
      (if (linSearch (car environment) symbol)
          (linSearchEnvironment (car environment) symbol)
          (linSearchEnvironment (cdr environment) symbol))))


;function that returns true if the environment pair that we are looking for was defined as a letrec expression, false otherwise
;this function makes the assumption that the symbol we are looking for exists in our list, so the function linSearch should be called and return true before this one is called
;environment -- the environment we want to search for symbol in
;symbol -- the variable/lexeme that we want to search for its attached value
(define (isLetrecExpression environment symbol)
  (if (equal? (car environment) symbol)
      (equal? (car (cdr (cdr (cdr environment)))) `$)
      (if (linSearch (car environment) symbol)
          (isLetrecExpression (car environment) symbol)
          (isLetrecExpression (cdr environment) symbol))))


;returns an entire environment (name, value, environment, and type) of the symbol we are looking for
;this function makes the assumption that the symbol we are looking for exists in our list, so the function linSearch should be called and return true before this one is called
;environment -- the environment we want to search for symbol in
;symbol -- the variable/lexeme that we want to search for its attached value
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

;tests for multiply nested lambda functions
(define nestedLambda
  `((lambda (a)
      ((lambda (b)
              ((lambda (c)
                 ((lambda (d)
                    ((lambda (e)
                       (* (* (* (* e d) c) b) a))
                     5))
                  4))
               3))
       2))
    1))

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

