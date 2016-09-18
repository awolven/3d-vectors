#|
 This file is a part of 3d-vectors
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:3d-vectors-test
  (:nicknames #:org.shirakumo.flare.vector.test)
  (:use #:cl #:parachute #:3d-vectors))
(in-package #:org.shirakumo.flare.vector.test)

(defun ~= (a b)
  (<= (abs (- a b)) 0.0001))

(defun va= (a b)
  (and (v= (vxy a) (vxy b))
       (v= (vxyz a) (vxyz b))
       (v= (vxyzw a) (vxyzw b))))

(defmacro isv-type-eq-error (op)
  `(progn (fail (,op (vec 1 2) (vec 1 2 3)))
          (fail (,op (vec 1 2) (vec 1 2 3 4)))
          (fail (,op (vec 1 2 3) (vec 1 2 3 4))))) 

(define-test 3d-vectors)

(define-test struct
  :parent 3d-vectors
  (of-type vec2 (vec2 1 2))
  (of-type vec3 (vec3 1 2 3))
  (of-type vec4 (vec4 1 2 3 4))
  (of-type vec2 (vec 1 2))
  (of-type vec3 (vec 1 2 3))
  (of-type vec4 (vec 1 2 3 4))
  (of-type vec2 (vcopy (vec 1 2)))
  (of-type vec3 (vcopy (vec 1 2 3)))
  (of-type vec4 (vcopy (vec 1 2 3 4)))
  (true (vec2-p (vec2 1 2)))
  (true (vec3-p (vec3 1 2 3)))
  (true (vec4-p (vec4 1 2 3 4)))
  (is = 1 (vx (vec 1 2)))
  (is = 2 (vy (vec 1 2)))
  (fail (vz (vec 1 2)))
  (fail (vw (vec 1 2)))
  (is = 1 (vx (vec 1 2 3)))
  (is = 2 (vy (vec 1 2 3)))
  (is = 3 (vz (vec 1 2 3)))
  (fail (vw (vec 1 2 3)))
  (is = 1 (vx (vec 1 2 3 4)))
  (is = 2 (vy (vec 1 2 3 4)))
  (is = 3 (vz (vec 1 2 3 4)))
  (is = 4 (vw (vec 1 2 3 4))))

(define-test comparators
  :parent 3d-vectors
  :depends-on (struct)
  (true (v= (vec 1 2) (vec 1 2)))
  (true (v= (vec 1 2 3) (vec 1 2 3)))
  (true (v= (vec 1 2 3 4) (vec 1 2 3 4)))
  (false (v= (vec 1 1) (vec 1 2)))
  (false (v= (vec 1 1 1) (vec 1 2 3)))
  (false (v= (vec 1 1 1 1) (vec 1 2 3 4)))
  (isv-type-eq-error v=)
  (true (v/= (vec 2 2) (vec 1 2)))
  (true (v/= (vec 2 2 3) (vec 1 2 3)))
  (true (v/= (vec 2 2 3 4) (vec 1 2 3 4)))
  (false (v/= (vec 1 2) (vec 1 2)))
  (false (v/= (vec 1 2 3) (vec 1 2 3)))
  (false (v/= (vec 1 2 3 4) (vec 1 2 3 4)))
  (isv-type-eq-error v/=)
  (true (v< (vec 1 1) (vec 2 2)))
  (true (v< (vec 1 1 1) (vec 2 2 2)))
  (true (v< (vec 1 1 1 1) (vec 2 2 2 2)))
  (false (v< (vec 1 1) (vec 1 1)))
  (false (v< (vec 1 1 1) (vec 1 1 1)))
  (false (v< (vec 1 1 1 1) (vec 1 1 1 1)))
  (isv-type-eq-error v<)
  (true (v> (vec 2 2) (vec 1 1)))
  (true (v> (vec 2 2 2) (vec 1 1 1)))
  (true (v> (vec 2 2 2 2) (vec 1 1 1 1)))
  (false (v> (vec 1 1) (vec 1 1)))
  (false (v> (vec 1 1 1) (vec 1 1 1)))
  (false (v> (vec 1 1 1 1) (vec 1 1 1 1)))
  (isv-type-eq-error v>)
  (true (v<= (vec 1 1) (vec 2 2)))
  (true (v<= (vec 1 1) (vec 1 1)))
  (true (v<= (vec 1 1 1) (vec 2 2 2)))
  (true (v<= (vec 1 1 1) (vec 1 1 1)))
  (true (v<= (vec 1 1 1 1) (vec 2 2 2 2)))
  (true (v<= (vec 1 1 1 1) (vec 1 1 1 1)))
  (false (v<= (vec 2 2) (vec 1 1)))
  (false (v<= (vec 2 2 2) (vec 1 1 1)))
  (false (v<= (vec 2 2 2 2) (vec 1 1 1 1)))
  (isv-type-eq-error v<=)
  (true (v>= (vec 2 2) (vec 1 1)))
  (true (v>= (vec 1 1) (vec 1 1)))
  (true (v>= (vec 2 2 2) (vec 1 1 1)))
  (true (v>= (vec 1 1 1) (vec 1 1 1)))
  (true (v>= (vec 2 2 2 2) (vec 1 1 1 1)))
  (true (v>= (vec 1 1 1 1) (vec 1 1 1 1)))
  (false (v>= (vec 1 1) (vec 2 2)))
  (false (v>= (vec 1 1 1) (vec 2 2 2)))
  (false (v>= (vec 1 1 1 1) (vec 2 2 2 2)))
  (isv-type-eq-error v>=))

(define-test swizzling
  :parent 3d-vectors
  :depends-on (comparators)
  ;; I'm not going to enumerate all possibilities, fuck that.
  (is v= (vec 2 2 2 2) (vorder (vec 1 2 3 4) :y :y :y :y))
  (is v= (vec 3 0 0 0) (vorder (vec 1 2 3 4) :z))
  (is v= (vec 1 1)     (vxx (vec 1 2 3 4)))
  (is v= (vec 1 2 3)   (vxyz (vec 1 2 3 4)))
  (is v= (vec 2 2 2 0) (vyyy_ (vec 1 2 3 4))))

(define-test constants
  :parent 3d-vectors
  :depends-on (comparators)
  (is v= (vec 1 0) +vx2+)
  (is v= (vec 0 1) +vy2+)
  (is v= (vec 1 0 0) +vx3+)
  (is v= (vec 0 1 0) +vy3+)
  (is v= (vec 0 0 1) +vz3+)
  (is v= (vec 1 0 0 0) +vx4+)
  (is v= (vec 0 1 0 0) +vy4+)
  (is v= (vec 0 0 1 0) +vz4+)
  (is v= (vec 0 0 0 1) +vw4+)
  (is v= (vec 1 0 0) +vx+)
  (is v= (vec 0 1 0) +vy+)
  (is v= (vec 0 0 1) +vz+))

(define-test arithmetic
  :parent 3d-vectors
  :depends-on (comparators swizzling)
  ;; FIXME: Tests for the modifying variants
  (is va= (vec 1 2 3 4) (v+ (vec 1 2 3 4)))
  (is va= (vec 3 4 5 6) (v+ (vec 1 2 3 4) 2))
  (is va= (vec 2 4 6 8) (v+ (vec 1 2 3 4) (vec 1 2 3 4)))
  (isv-type-eq-error v+)
  (isv-type-eq-error nv+)
  (is va= (vec -1 -2 -3 -4) (v- (vec 1 2 3 4)))
  (is va= (vec -1 0 1 2) (v- (vec 1 2 3 4) 2))
  (is va= (vec 0 0 0 0) (v- (vec 1 2 3 4) (vec 1 2 3 4)))
  (isv-type-eq-error v-)
  (isv-type-eq-error nv-)
  (is va= (vec 1 2 3 4) (v* (vec 1 2 3 4)))
  (is va= (vec 2 4 6 8) (v* (vec 1 2 3 4) 2))
  (is va= (vec 1 4 9 16) (v* (vec 1 2 3 4) (vec 1 2 3 4)))
  (isv-type-eq-error v*)
  (isv-type-eq-error nv*)
  (is va= (vec 1 1/2 1/3 1/4) (v/ (vec 1 2 3 4)))
  (is va= (vec 1/2 1 3/2 2) (v/ (vec 1 2 3 4) 2))
  (is va= (vec 1 1 1 1) (v/ (vec 1 2 3 4) (vec 1 2 3 4)))
  (isv-type-eq-error v/)
  (isv-type-eq-error nv/)
  (is va= (vec 2 3 4 5) (v1+ (vec 1 2 3 4)))
  (is va= (vec 0 1 2 3) (v1- (vec 1 2 3 4))))

(define-test vector-math
  :parent 3d-vectors
  :depends-on (comparators)
  (is = 1 (vlength (vec 1 0 0 0)))
  (is = 4 (vlength (vec 4 0 0 0)))
  (is = (sqrt (+ 1 4 9 16)) (vlength (vec 1 2 3 4)))
  (is = (+ (* 1 4) (* 2 3) (* 3 2) (* 4 1)) (v. (vec 1 2 3 4) (vec 4 3 2 1)))
  (is v= (vec -4 8 -4) (vc (vec 1 2 3) (vec 3 2 1)))
  (is v= (vec 3.0301156 2.0 0.90465444) (vrot (vec 1 2 3) (vec 0 1 0) (sin 360)))
  (is ~= (vlength (vunit (vec 1 2 3 4))) 1)
  (is ~= (vlength (vscale (vec 1 2 3 4) 2)) 2)
  (is v= (vec 1 1 2 2) (vabs (vec 1 -1 -2 2)))
  (is v= (vec 5 0 5 0) (vmod (vec 5 10 15 20) 10))
  (is v= (vec -1 0 2 2) (vclamp -1 (vec -2 0 2 3) 2))
  (is v= (vec -1 -1 1 1) (vlimit (vec -2 -1 1 2) 1)))

(define-test modifiers
  :parent 3d-vectors
  :depends-on (comparators)
  (let ((vec (vec 1 2 3 4)))
    (is v= (vec 4 3 2 1) (vsetf vec 4 3 2 1))
    (is v= (vec 5 4 3 2) (vapply vec 1+))
    (is v= (vec 3 2 1 0) (vapplyf vec 1-))
    (is v= (vec 8 7 6 5) (vincf vec 5))
    (is v= (vec 6 5 4 3) (vdecf vec 2))))
