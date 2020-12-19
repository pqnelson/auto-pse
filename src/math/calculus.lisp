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
    :reader integrand ;; there is no other use of the word "integrand"
    :initform (error "Must supply an integrand."))
   (variable
    :initarg :variable
    :reader integral-variable
    :initform (error "Must supply a variable of integration."))
   (domain
    :initform nil
    :documentation "Domain of integration. NIL means indefinite integral."))
  (:documentation "Lazy integration of an INTEGRAND with respect to a VARIABLE, possibly over some DOMAIN if finite (otherwise indefinite)."))

;; Examples of what I'd like integrate to do:
;; (integrate '(sin x) x) => '(+ (cos x) (constant-of-integration))
;; (integrate '(sin (+ x y)) (x y)) => '(+ (* c1 x) c2 (- (sin (+ x y))))
;; (integrate '(sin (+ x y)) ((x 0 1) (y 1 pi)))
;; (integrate '(sin (+ x y)) (x y) :in <region>)

;; TODO: make optional additional variables for multivariate integrals
;; TODO: handle `(in (x y ...) region-description)`?
(defgeneric integrate (f variable)
  (:documentation "Integrate a function F with respect to VARIABLE."))


;; TODO: check if f contains the variable of integration. If not, return
;;       the answer immediately (+ (* variable f) C).
;; TODO: check if f is a polynomial, which can be solved in closed-form.
(defmethod integrate (f variable)
  (declare (type symbol variable))
  ;; (if (free-var? (variable f)) `(+ (* ,variable ,f) ,(gensym "C"))
  (make-instance 'integral
                 :integrand f
                 :variable variable))
;; )

;; I suppose if I were a horrible human being, I would overload this to also
;; handle variational derivatives, functional derivatives, Lie derivatives,
;; Gateau derivatives, ...
(defgeneric d (f variable)
  (:documentation "Differentiate F with respect to VARIABLE."))

(defgeneric contains-var? (expr var))

(defmethod contains-var? ((expr symbol) var)
  (eq var expr))

(defmethod contains-var? ((expr list) var)
  (some #'(lambda (term)
            (contains-var? term var))
        expr))

(defmethod contains-var? ((expr integral) var)
  (unless (eq (integral-variable expr) var)
    (contains-var? (integrand expr))))

(defun free-var? (var expr)
  (not (contains-var? expr var)))

(defmethod d ((f symbol) variable)
  (if (eq f variable)
      1
      0))

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
