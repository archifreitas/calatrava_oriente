#lang racket

(provide (all-defined-out))
(require (prefix-in truss: "TrussDef.rkt"))
(require "Functions.rkt")


#|------------------------------------------------------------------|#

(require rosetta)
(require (prefix-in geo: rosetta/autocad)) 

#|---------------------------Surfaces-------------------------------|#

#| ROOF |#   

#| Roof Shape |#

(define (roof-shape p0 p1 p2)
  (geo:extrusion (geo:surface-polygon p0 p1 p2) 0.01))

#| Roof Plate |#


(define (roof-plate p pa pb beam-height mini-radius n)
  (roof-shape(geo:+z p beam-height) pa pb))

#| Roof |#

(define (roof p p-list beam-height med-radius mini-radius n)
  (for/list ([num (range (- (length p-list) 1))])
    (roof-plate p
                (list-ref p-list num)
                (list-ref p-list (+ 1 num))
                beam-height
                mini-radius
                n))
  (roof-plate p (last p-list) (list-ref p-list 0) beam-height mini-radius n))

#| Roof Grid |#

(define (roof-grid grid width beam-height inner-height outer-height radius med-radius mini-radius div n)
  (let* ((flat-list (apply append grid)))
    (for/list ([pt flat-list])
      (roof pt
            (points pt width inner-height outer-height beam-height)
            beam-height
            med-radius
            mini-radius
            n))))

#|----------------------------Bars/Beams-------------------------------|#

#| ROOF |#   
#| Rods |#

(define (rods p pa pb mini-radius n program)
  (let* ((result0 (list-div p pa n))
         (result1 (list-div p pb n)))
    (for/list ([num (range 1 (length result0))])
      (case program
        ((cad) (truss:c-bar (list-ref result0 num) mini-radius (list-ref result1 num)))
        ((robot) (truss:node (list-ref result0 num))
                 (truss:node (list-ref result1 num))
                 (truss:r-bar (list-ref result0 num) (list-ref result1 num)))))))

#| Rod |#

(define (rod centro-base radius topo program)
  (case program
    ((cad) (truss:c-bar centro-base radius topo))
    ((robot) (truss:node centro-base)
             (truss:node topo)
             (truss:r-bar centro-base topo))))


#| Roof Plate |#

(define (roof-rods p pa pb beam-height mini-radius n program)
  (rods(+z p beam-height) pa pb mini-radius n program))


#| Roof |#

(define (t-roof-rods p p-list beam-height med-radius mini-radius n program)
  (for/list ([num p-list])
    (cond ((= (remainder (index-of p-list num) 2) 0)
          (rod(+z p beam-height) med-radius num program))
          (else
           (rod(+z p beam-height) mini-radius num program))))
  (for/list ([num (range (- (length p-list) 1))])
    (roof-rods p (list-ref p-list num) (list-ref p-list (+ 1 num)) beam-height mini-radius n program))
  (roof-rods p (last p-list) (list-ref p-list 0) beam-height mini-radius n program))

#| Arc Beam |#

(define (arc-beams p width beam-height inner-height mini-radius div n program)
  (case program
    ((cad) (for/list ([t (division 0 2pi 4 #f)])
             (let*((e-list (elipse-list p (/ width 2) (+ inner-height (- beam-height mini-radius)) t div n)))
               (for/list ([num (range(- (length e-list) 1))])
                        (truss:c-bar (list-ref e-list num) mini-radius (list-ref e-list (+ 1 num)))))))
    ((robot) (truss:fixed-node p)
             (for/list ([t (division 0 2pi 4 #f)])
               (let*((e-list (elipse-list p (/ width 2) (+ inner-height beam-height) t div n)))
                 (for/list ([num (range (- (length e-list) 1))])
                   (truss:node (list-ref e-list (+ 1 num))))
                 (truss:r-bar p (list-ref e-list 1))
                 (for/list ([num (range 1 (- (length e-list) 1))])
                   (truss:r-bar (list-ref e-list num) (list-ref e-list (+ 1 num)))))))))

#| Beam to Roof |#
(define (beam-to-roof p width beam-height inner-height outer-height radius mini-radius phi div n program)
  (let* ((e-list (elipse-list p (/ width 2) (+ inner-height beam-height) phi div n))
         (r1-list (list-div (+z p beam-height) (+cyl p
                                                     (* (/ width 2) (sqrt 2))
                                                     (+ phi (+ pi pi/4))
                                                     (+ outer-height beam-height)) n))
         (r2-list (list-div (+z p beam-height) (+cyl p
                                                     (* (/ width 2) (sqrt 2))
                                                     (+ phi (- pi pi/4))
                                                     (+ outer-height beam-height)) n)))
    (for/list ([num (range(length e-list))])
      (case program
          ((cad) (truss:c-bar (list-ref e-list num) mini-radius (list-ref r1-list num))
                 (truss:c-bar (list-ref e-list num) mini-radius (list-ref r2-list num)))
        ((robot) (truss:node (list-ref e-list num))
                 (truss:node (list-ref r1-list num))
                 (truss:node (list-ref r2-list num))
                 (truss:r-bar (list-ref e-list num) (list-ref r1-list num))
                 (truss:r-bar (list-ref e-list num) (list-ref r2-list num)))))))

#| Beams Distribution |#

(define (beam-grid grid width beam-height inner-height outer-height radius med-radius mini-radius div n program)
  (let* ((flat-list (apply append grid)))
    (for/list ([pt flat-list])
      (arc-beams pt width beam-height inner-height radius div n program)
      (t-roof-rods pt (points pt width inner-height outer-height beam-height) beam-height med-radius mini-radius n program)
      (for/list ([t (division 0 2pi 4 #f)])
        (beam-to-roof pt width beam-height inner-height outer-height radius mini-radius t div n program)))))

#|------------------------------------------------------------------|#

#| Beams Tests |#

(define (testrod)
  (rods (xyz 0 0 0) (xyz 5 5 0) (xyz 5 0 0) 0.01 10 'robot))

#| Surfaces Tests |#

(define (runsurf)
  (roof-grid (pts-grid 20 2 4) 20 10 1 0 0.3 0.1 0.05 1.80 15))