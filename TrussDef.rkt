#lang racket

(provide (all-defined-out))

#|------------------------------------------------------------------|#

(require rosetta/robot/backend)
(require (prefix-in geo: rosetta/autocad)) 

#|------------------------------------------------------------------|#

#| Families |#

(define fixed-truss-node-family
  (truss-node-family-element
   (default-truss-node-family)
   #:support (create-node-support "FixedSup"
                                  #:ux #t
                                  #:uy #t
                                  #:uz #t)))

(define truss-bar-family
  (truss-bar-family-element
   (default-truss-bar-family)
   #:material (list "S460"                     ;material code
                    I_MT_STEEL                 ;material type
                    "Steel"                    ;material name type
                    "I'm really steel"         ;material nuance
                    210000000000.0             ;young modulus
                    0.3                        ;poisson coefficient
                    81000000000.0              ;shear module
                    76500.0                    ;specific weight
                    1.2E-05                    ;coefficient of thermal expansion
                    0.04                       ;damping ratio
                    460000000.0                ;yield strenght
                    540000000.0)               ;limit tenstion resistance
   #:section (list "PIPE_S460"                 ;section name code
                   "S460"                      ;code
                   #f                          ;is the material wood?
                   (list (list #f              ;is the section solid?
                               0.15            ;diameter of tubular section
                               0.07)))))       ;thickness of tubular section

#| Robot Functions |#

(define (node p)
  ;(printf "p = ~a\n" p)
  (truss-node p))

(define (fixed-node p)
  ;(printf "fixed-p = ~a\n" p)
  (truss-node p fixed-truss-node-family))

(define (r-bar p0 p1)
  ;(printf "p0 = ~a p1 = ~a\n" p0 p1)
  (truss-bar p0 p1 #f truss-bar-family))
 
#| CAD Functions |#

(define (c-bar p0 raio p1)
  (geo:cylinder p0 raio p1))
