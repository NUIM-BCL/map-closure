;;; $Id: ex1.sc,v 1.3 2006/12/14 22:37:02 qobi Exp $

(define (lift-+ +)
 (lambda (x y)
  (if (pair? x)
      (if (pair? y)
          (cons (+ (car x) (car y))
                (+ (cdr x) (cdr y)))
          (cons (+ (car x) y) (cdr x)))
      (if (pair? y)
          (cons (+ x (car y)) (cdr y))
          (cons (+ x y) 0)))))

(define + (lift-+ +))

(define (f x) (+ x x))

(define g (let ((y 10)) (lambda (x) (+ x y))))

(define h (let ((p +)) (lambda (x) (p x 5))))

(write (f '(2 . 3)))
(newline)

(write (g '(1 . 2)))
(newline)

(write (h '(1 . 2)))
(newline)
