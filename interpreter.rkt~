#lang racket

(define (startEval prog)
  (if (not (pair? prog))
      prog
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
                          0)))))))


(define (arithmaticExpr Expr)
  (case (car Expr)
    (`+ (+ (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    (`- (- (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    (`* (* (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    (`/ (/ (startEval (car (cdr Expr))) (startEval (car (cdr (cdr Expr))))))
    ))

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

(define (ifEval Expr)
  (if (startEval (car (cdr Expr)))
      (startEval (car (cdr (cdr Expr))))
      (startEval (car (cdr (cdr (cdr Expr)))))))

(define (arithmaticExpr? expr)
  (or (or (or (equal? expr `+) (equal? expr `-)) (equal? expr `*)) (equal? expr `/)))

(define (relationalExpr? expr)
  (or (or (or (or (or (equal? expr `=) (equal? expr `<=)) (equal? expr `<)) (equal? expr `>=)) (equal? expr `>)) (equal? expr `equal?)))

(define (listOperator? expr)
  (or (or (or (equal? expr `cons) (equal? expr `car)) (equal? expr `cdr)) (equal? expr `pair?)))