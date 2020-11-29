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
(defpackage #:auto-pse.calculus
  (:use #:cl)
  (:export integrate))
(in-package #:auto-pse.calculus)

(defclass integral ()
  ((integrand
    :initarg :integrand
    :reader integrand
    :initform (error "Must supply an integrand."))
   (variable
    :initarg :variable
    :initform (error "Must supply a variable of integration."))
   (domain
    :initform nil
    :documentation "Domain of integration. NIL means indefinite integral.")))

;; TODO: make optional additional variables for multivariate integrals
;; TODO: handle `(in (x y ...) region-description)`?
(defgeneric integrate (f variable)
  (:documentation "Integrate a function F with respect to VARIABLE."))

;; TODO: check if f contains the variable of integration. If not, return
;;       the answer immediately (+ (* variable f) C).
;; TODO: check if f is a polynomial, which can be solved in closed-form.
(defmethod integrate (f variable)
  (declare (type symbol variable))
  (make-instance 'integral
                 :integrand f
                 :variable variable))

;; I suppose if I were a horrible human being, I would overload this to also
;; handle variational derivatives, functional derivatives, Lie derivatives,
;; Gateau derivatives, ...
(defgeneric d (f variable)
  (:documentation "Differentiate F with respect to VARIABLE."))

(defun contains-var? (expr var)
  ;; (labels ((contains? ()))
  ;;   )
  )

(defun free-var? (var expr)
  (not (contains-var? expr var)))

(defmethod d ((f integral) variable)
  (cond
    ((eq (integral-variable f) variable) (integral-integrand f))
    ((free-var? variable (integral-integrand f)) 0)
    (t ;; (contains-var (integrand f) variable)
     (integrate (d (integral-integrand f) variable)
                (integral-variable f)))
    ))

;; (defthm fundamental-calculus
;;     (equal (d (integrate (f x) x) x)
;;            (f x)))

;;;; Numerical methods for integration
(defparameter quadrature-method nil
  "Determines which numerical method to perform quadratures.")
