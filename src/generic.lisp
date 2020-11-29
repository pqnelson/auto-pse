;;;; This file is part of auto-pse.
;;;;
;;;; auto-pse is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; auto-pse is distributed in the hope it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU general Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with auto-pse. If not, see <https://www.gnu.org/licenses>
(defpackage #:auto-pse.generic
  (:use #:cl)
  (:shadow
   + - / * expt                 ;... n-ary arith
   = /= > < <= >=               ;... n-ary comparisons
   sin cos tan
   atan asin acos atan2
   sinh cosh tanh
   asinh acosh atanh
   expt log exp sqrt
   min max
   1- 1+ abs incf decf
   numerator denominator
   ffloor fceiling ftruncate fround
   ash evenp oddp
   scale-float
   ;;true false 
   )
  
  )
(in-package #:auto-pse.generic)

(defgeneric inexact? (x))

(defmethod inexact? ((x single-float))
  t)

(defmethod inexact? ((x double-float))
  t)

(defmethod inexact? ((x complex))
  (or (inexact? (realpart x))
      (inexact? (imagpart x))))

(defmethod inexact? ((x number))
  nil)

(defgeneric zero-like (x)
  (:documentation "Produces the 'zero' for the given type."))

(defmethod zero-like ((x number))
  0)

(defgeneric one-like (x)
  (:documentation "The multiplicative identity for the given type."))

(defmethod one-like ((x number))
  1)

(defmethod one-like ((x t)))

(defgeneric zero? (x)
  (:documentation "Tests if the given quantity is ZERO-LIKE."))

(defmethod zero? ((x number))
  (zerop x))

(defmethod zero? ((x t)))

(defgeneric one? (x))

(defmethod one? ((x number))
  (= 1 x))

(defmethod one? (x))

(defgeneric negate (x))
(defgeneric invert (x))
(defgeneric sqrt (x))
(defgeneric exp (x))
(defgeneric log (x))
(defgeneric sin (x))
(defgeneric cos (x))
(defgeneric tan (x))
(defgeneric asin (x))
(defgeneric acos (x))
(defgeneric atan (x))
(defgeneric sinh (x))
(defgeneric cosh (x))
(defgeneric abs (x))
(defgeneric determinant (x))
(defgeneric trace (x))
(defgeneric transpose (x))
(defgeneric dimension (x))

(defgeneric add (x y))

(defmethod add ((x number) (y number))
  (cl::+ x y))

(defmethod add ((x t) y)
  (cond
    ((zero? x) y)
    ((zero? y) x)
    ((numberp y) (list 'add y x))
    (t (list 'add x y))))

(defgeneric sub (x y))

(defmethod sub ((x number) (y number))
  (cl::- x y))

(defmethod sub (x y)
  (cond
    ((zero? x) (negate y))
    ((zero? y) x)
    (t (list 'sub x y))))

(defgeneric multiply (x y))

(defmethod multiply ((x number) (y number))
  (cl::* x y))

(defmethod multiply (x y)
  (cond
    ((zero? x) (zero-like x))
    ((zero? y) (zero-like y))
    ((one? x) y)
    ((one? y) x)
    (t (list 'multiply x y))))

(defgeneric divide (x y))

(defmethod divide ((x number) (y number))
  (cl::/ x y))

(defmethod divide (x y)
  (cond
    ((one? y) x)
    (t (list 'divide x y))))

(defgeneric pow (x y))

(defmethod pow ((x number) (y number))
  (cl:expt x y))

(defmethod pow (x y)
  (cond
    ((one? x) x)
    ((zero? y) (one-like x))
    ((one? y) x)
    (t (list 'pow x y))))


