#lang racket

(provide (all-defined-out))
(require "Parameters.rkt")

#|------------------------------------------------------------------|#

(require rosetta)

#|------------------------------------------------------------------|#

#| Grid |#

(define (pts-grid width nl nw)
  (map-division (lambda (a b) (xy a b))
                0 (* width nl) nl #f
                0 (* width nw) nw #f))

#| Flatten |#

(define (zip l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else (cons (car l1) (cons (car l2) (zip (cdr l1) (cdr l2)))))))

#| Points |#

(define (points p width inner-height outer-height beam-height)
    (let*
        ((list-a (for/list ([t (division -pi pi 4 #f)])
              (+cyl p (/ width 2) t (+ inner-height beam-height))))
         (list-b (for/list ([t (division -pi pi 4 #f)])
              (+cyl p (* (/ width 2) (sqrt 2)) (+ pi/4 t) (+ outer-height beam-height)))))
      (zip list-a list-b)))

#| -1 0 1 |#

(define (sgn x)
  (cond ((< x 0) -1)
        ((= x 0) 0)
        (else 1)))

#| Divide distance between two points |#

(define (list-div p pf n)
  (let*
    ((list-x (division (cx p) (cx pf) n))
     (list-y (division (cy p) (cy pf) n))
     (list-z (division (cz p) (cz pf) n)))
     (for/list ([num (range (length list-x))])
       (xyz (list-ref list-x num)
            (list-ref list-y num)
            (list-ref list-z num)))))

#| Pontos Elipse |#

(define (pts-elipse p width b phi div t)
  (let* ((ponto1 (+cyl p width (- phi) 0))
         (ponto2 (+cyl p (- width) phi 0)))
    (cond ((= phi (or pi/2 3pi/2))
          (+cyl ponto1
                (* width
                   (expt (expt (cos t) 2) (/ 1 div))
                   (sgn (cos t)))
                phi
                (* b
                   (expt (expt (sin t) 2) (/ 1 div))
                   (sgn (sin t)))))
        (else
         (+cyl ponto2
               (* width
                  (expt (expt (cos t) 2) (/ 1 div))
                  (sgn (cos t)))
               phi
               (* b
                  (expt (expt (sin t) 2) (/ 1 div))
                  (sgn (sin t))))))))
  

#| Quarter Elipse |#

(define (elipse-list p width b phi div n)
  (for/list ([t (division 0 pi/2 n)])
    (pts-elipse p width b phi div t)))