#lang racket

(define (startEval prog)
  (if (ArithmaticExpr? (car prog))
      (ArithExpr prog)
      (if (RelationalExpr? (car prog))
          (RelationalExpr prog)
          (if (equal? (car prog) `if)
              (IfEval prog)
              0))))
      
;function to evaluate an arithmatic expression
(define (ArithExpr Expr)
  (if (not (pair? Expr))
      ;just return the number if it is an atom (i.e. it is just a number)
      Expr
      (if (equal? (car Expr) `+)
          ;add the numbers together and return their sum
          (+ (ArithExpr (car (cdr Expr))) (ArithExpr (car (cdr (cdr Expr)))))
          (if (equal? (car Expr) `-)
              ;subtract the numbers and return their difference
              (- (ArithExpr (car (cdr Expr))) (ArithExpr (car (cdr (cdr Expr)))))
              (if (equal? (car Expr) `*)
                  ;multiply the numbers together and return their product
                  (* (ArithExpr (car (cdr Expr))) (ArithExpr (car (cdr (cdr Expr)))))
                  ;divide the numbers and return their quotient
                  (/ (ArithExpr (car (cdr Expr))) (ArithExpr (car (cdr (cdr Expr))))))))))


;function to evaluate a relational expression
(define (RelationalExpr Expr)
  (if (equal? (car Expr) `=)
      (= (ArithExpr (car (cdr Expr))) (ArithExpr (car (cdr (cdr Expr)))))
      (if (equal? (car Expr) `<=)
          (<= (ArithExpr (car (cdr Expr))) (ArithExpr (car (cdr (cdr Expr)))))
          (if (equal? (car Expr) `<)
              (< (ArithExpr (car (cdr Expr))) (ArithExpr (car (cdr (cdr Expr)))))
              (if (equal? (car Expr) `>=)
                  (>= (ArithExpr (car (cdr Expr))) (ArithExpr (car (cdr (cdr Expr)))))
                  (> (ArithExpr (car (cdr Expr))) (ArithExpr (car (cdr (cdr Expr))))))))))

(define (IfEval Expr)
  (if (RelationalExpr (car (cdr Expr)))
      (startEval (car (cdr (cdr Expr))))
      (startEval (car (cdr (cdr (cdr Expr)))))))



(define (ArithmaticExpr? expr)
  (or (or (or (equal? expr `+) (equal? expr `-)) (equal? expr `*)) (equal? expr `/)))

(define (RelationalExpr? expr)
  (or (or (or (or (equal? expr `=) (equal? expr `<=)) (equal? expr `<)) (equal? expr `>=)) (equal? expr `>)))