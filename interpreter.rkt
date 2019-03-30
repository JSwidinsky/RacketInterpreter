#lang racket
(require racket/trace)

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
          (if (pair? prog)
              (if (not (empty? (cdr prog)))
                  (startEvalList (append (list (linSearchVal environment (car prog))) (map (lambda (x y) (startEvalList x y)) (cdr prog) (make-list (length (cdr prog)) environment))) (linSearchEnvironment environment (car prog))) ;i literally changed this line and now remove doesn't work
                  (startEvalList (list (linSearchVal environment (car prog))) (linSearchEnvironment environment (car prog))));perform a start eval here in the case where a parameter is defined in terms of another parameter
              (startEvalList (linSearchVal environment prog) (linSearchEnvironment environment prog)))
          (if (not (pair? prog))
              prog
              (if (empty? (cdr prog))
                  (startEvalList (car prog) environment)   ;this used to be (cdr prog); can I remember why? (changed it so that car would work with input `(6))
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
  (startEvalList (car (cdr (cdr expr))) (cons (modifyLetEnvironment (car (cdr expr)) environment) environment))) ;(cons (modifyLetEnvironment (car (cdr expr)) environment) environment)))

(define (letrecEval expr environment)
  (startEvalList (car (cdr (cdr expr))) (cons (modifyLetrecEnvironment (car (cdr expr)) environment) environment)));(cons (car (cdr expr)) environment)))
   
;helper function that returns a modified list for an updated environment given parameters of a lambda function and the arguments to that function
;params -- our variable names for our new environment
;arguments -- the values of the variables
;environment -- current environment that will be modified
(define (modifyEnvironment params arguments environment)
  (if (= (length params) 1)
      (list (car params) (startEvalList (if (pair? arguments) (car arguments) arguments) environment) environment)
      (list (list (car params) (startEvalList (car arguments) environment) environment) (modifyEnvironment (cdr params) (cdr arguments) environment) environment)))

(define (modifyLetEnvironment params environment)
  (if (= (length params) 1)
      (list (car (car params)) (car (cdr (car params))) environment)
      (list (list (car (car params)) (car (cdr (car params))) environment) (modifyLetEnvironment (cdr params) environment))))

(define (modifyLetrecEnvironment params environment)
  (if (= (length params) 1)
      (list (car (car params)) (car (cdr (car params))) (list (car (car params)) (car (cdr (car params))) environment))
      (list (list (car (car params)) (car (cdr (car params))) (list (car (car params)) (car (cdr (car params))) environment)) (modifyLetEnvironment (cdr params) environment))))

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






;put any sample test programs here
(define sampleProg `(cons 6 (cdr (5 8))))  ;this may not work quite right yet?
(define s `(car (6)))
(define st `(if (< 6 (+ 4 1))
                (equal? (car (4 5)) (cdr (5 4)))
                (+ (* 6 7) (- (/ 10 5) 1))))

(define s3 `(if ((lambda (x y) (< x y)) 4 5) ((lambda (a b) (+ a (* b b))) 2 5) (+ 4 5)))
(define s4 `((lambda (x y z) (if (< x y) (+ x y) (* x y (+ z 7)))) 8 9 0))
(define s5 `((lambda (x) ((lambda (a) (* a x)) x)) 5))

(define lam `((lambda (x y) (+ ((lambda (x y) (* x y)) (+ x y) 5) y)) (+ 3 4) (- 4 5)))
(define lamb `((lambda (+) (+ 5 6)) *))

(define ex `(if (< ((lambda (x) (* x x)) 5) ((lambda (x) (* x (* x x))) 4)) (quote 4) (quote 9)))

(define letExpr `(let ((x 3) (y 4)) (let ((x y)) (+ x y))))

(define letexpr2 `(let ((y 5)) (let ((f (lambda (x) (* x y)))) (let ((y 3)) (f y)))))

(define f1
  '(let ((inc
          (lambda (x) (+ x (quote 1)))))
     (inc (quote 5)))
  ) ;should be 6

(define fact
  '(letrec ((fact
             (lambda (x)
               (if (= x 0) (quote 1)
                   (* x (fact (- x 1)))))))
     (fact 10))
  ) ;should be 3628800

(define fib
  '(letrec ((fib
             (lambda (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))
     (fib 7))
  ) ;should be 21


(define intersect
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
  );should be (b c d)

(define remove
  '(let ((+ (lambda (x) (cdr x)))
         (- (quote (1 2 3 4 5))))
     (+ -))
  ) ;should be (2 3 4 5)

(define nestedLet
  '(let ((y 10))
     (let ((f (lambda (x) (+ x y))))
       (let ((y 100))
         (f y)))))

(trace startEvalList)
(trace modifyEnvironment)