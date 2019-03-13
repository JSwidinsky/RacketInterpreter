#lang racket

(define (startEval prog)
  (if (not (pair? prog))
      prog
      (if (empty? (cdr prog))
          (car prog)
          (if (equal? (car prog) `quote)
              (quote (cdr prog))
              (if (arithmaticExpr? (car prog))
                  (arithmaticExpr prog)
                  (if (relationalExpr? (car prog))
                      (relationalExpr prog)
                      (if (listOperator? (car prog))
                          (listOperation prog)
                          (if (equal? (car prog) `if)
                              (ifEval prog)
                              0))))))))


;function that evaluates a given arithmatic expression, depending on what the car of the parameter Expr is (i.e. +, -, *, or /)
;Expr -- The arithmatic expression to be evaluated
(define (arithmaticExpr Expr)
  (case (car Expr)
    (`+ (+ (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    (`- (- (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    (`* (* (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    (`/ (/ (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    ))

;function that evaluates a given relational expression, and returns true or false depending on its value
;Expr -- The relational expression to evaluate
(define (relationalExpr Expr)
  (case (car Expr)
    (`equal? (equal? (car (cdr Expr)) (car (cdr (cdr Expr)))))
    (`= (= (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    (`<= (<= (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    (`< (< (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    (`>= (>= (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    (`> (> (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    ))

;this might not work yet...
(define (listOperation Expr)
  (case (car Expr)
    (`car (car (cdr Expr)))
    (`cdr (cdr (cdr Expr)))
    (`cons (cons (car (cdr Expr)) (car (cdr (cdr Expr)))))
    (`pair? (pair? Expr))
    ))

;function that evaluates an if expression, and depending on the result of its conditional branches to the appropriate part of the list
;Expr -- The expression that we wish to evaluate for the if statement
(define (ifEval Expr)
  (if (startEval (car (cdr Expr)))
      (startEval (car (cdr (cdr Expr))))
      (startEval (car (cdr (cdr (cdr Expr)))))))

;determines if the parameter expr is one of the four arithmatic operators
(define (arithmaticExpr? expr)
  (or (or (or (equal? expr `+) (equal? expr `-)) (equal? expr `*)) (equal? expr `/)))

;determines if the paramter expr is one of the six relational operators
(define (relationalExpr? expr)
  (or (or (or (or (or (equal? expr `=) (equal? expr `<=)) (equal? expr `<)) (equal? expr `>=)) (equal? expr `>)) (equal? expr `equal?)))

;determine if the parameter is one of the four list operators
(define (listOperator? expr)
  (or (or (or (equal? expr `cons) (equal? expr `car)) (equal? expr `cdr)) (equal? expr `pair?)))

(define sampleProg
  `(if (> 5 6)
       (+ 3 4)
       (if (> (- 7 9) (+ 4 3))
           (< 4 5)
           (car `(4 5)))))