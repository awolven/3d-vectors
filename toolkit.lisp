#|
 This file is a part of 3d-vectors
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:3d-vectors.f)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *float-type* 'single-float))

(deftype float-type ()
  '#.*float-type*)

(declaim (inline ensure-float))
(declaim (ftype (function (real) #.*float-type*)))
(defun ensure-float (thing)
  (declare (optimize (speed 1)))
  (coerce thing '#.*float-type*))

(defun ensure-float-param (val env)
  (if (constantp val env)
      (typecase val
        (real (ensure-float val))
        (T `(load-time-value (ensure-float ,val))))
      `(locally (declare (optimize (speed 1))) (ensure-float ,val))))

(defmacro define-ofun (name args &body body)
  `(progn
     #+sbcl (declaim (sb-ext:maybe-inline ,name))
     (defun ,name ,args
       (declare (optimize (compilation-speed 0) (debug 1) (safety 1) speed))
       ,@body)))

(defmacro defsetf* (name args values &body body)
  #-(or ccl abcl ecl)
  `(defsetf ,name ,args ,values ,@body)
  #+(or ccl abcl ecl) ;; Compiler bug workarounds, hooray.
  (let ((args (loop for arg in args
                    until (eql arg '&environment)
                    collect arg))
        (env (loop for arg = (pop args)
                   while arg
                   do (when (eql arg '&environment)
                        (return (pop args))))))
    `(defsetf ,name ,args ,values
       ,@(if env
             `((let (,env)
                 ,@body))
             body))))

(defun intern* (&rest parts)
  (let ((*print-case* (readtable-case *readtable*))
        (*package* #.*package*))
    (intern (format NIL "~{~a~}" parts) #.*package*)))

(defmacro define-ptrfun (name type first-slot)
  #+sbcl
  (let ((name-addr (intern* name 'addr)))
    `(progn
       (sb-c::define-structure-slot-addressor ,name-addr :structure ,type :slot ,first-slot)
       (declaim (inline ,name))
       (declaim (ftype (function (,type) sb-sys:system-area-pointer) ,name))
       (defun ,name (vec)
         (sb-sys:int-sap (,name-addr vec))))))

(in-package #:3d-vectors.d)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *float-type* 'double-float))

(deftype float-type ()
  '#.*float-type*)

(declaim (inline ensure-float))
(declaim (ftype (function (real) #.*float-type*)))
(defun ensure-float (thing)
  (declare (optimize (speed 1)))
  (coerce thing '#.*float-type*))

(defun ensure-float-param (val env)
  (if (constantp val env)
      (typecase val
        (real (ensure-float val))
        (T `(load-time-value (ensure-float ,val))))
      `(locally (declare (optimize (speed 1))) (ensure-float ,val))))

(defmacro define-ofun (name args &body body)
  `(progn
     #+sbcl (declaim (sb-ext:maybe-inline ,name))
     (defun ,name ,args
       (declare (optimize (compilation-speed 0) (debug 1) (safety 1) speed))
       ,@body)))

(defmacro defsetf* (name args values &body body)
  #-(or ccl abcl ecl)
  `(defsetf ,name ,args ,values ,@body)
  #+(or ccl abcl ecl) ;; Compiler bug workarounds, hooray.
  (let ((args (loop for arg in args
                    until (eql arg '&environment)
                    collect arg))
        (env (loop for arg = (pop args)
                   while arg
                   do (when (eql arg '&environment)
                        (return (pop args))))))
    `(defsetf ,name ,args ,values
       ,@(if env
             `((let (,env)
                 ,@body))
             body))))

(defun intern* (&rest parts)
  (let ((*print-case* (readtable-case *readtable*))
        (*package* #.*package*))
    (intern (format NIL "~{~a~}" parts) #.*package*)))

(defmacro define-ptrfun (name type first-slot)
  #+sbcl
  (let ((name-addr (intern* name 'addr)))
    `(progn
       (sb-c::define-structure-slot-addressor ,name-addr :structure ,type :slot ,first-slot)
       (declaim (inline ,name))
       (declaim (ftype (function (,type) sb-sys:system-area-pointer) ,name))
       (defun ,name (vec)
         (sb-sys:int-sap (,name-addr vec))))))
