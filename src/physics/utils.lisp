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
(defpackage #:auto-pse.physics.utils
  (:use #:cl pq)
  (:export time? length? mass? luminosity? temperature? electric-current?
           amount-of-substance?
           energy? power? force? weight? velocity? acceleration?))
(in-package #:auto-pse.physics.utils)

(defmacro defdimen (name (qty) base-unit)
  (let ((units (gensym "units-")))
      `(defun ,name (,qty)
         (declare (type (or physical-quantities:quantity
                            (cons pq::unit-factor))
                        ,qty))
         (let ((,units (pq::expand-unit (if (consp ,qty)
                                            ,qty
                                            (pq:unit ,qty)))))
           (pq:units-convertible ,base-unit ,units)))))

;;; fundamental dimensions
(defdimen time? (qty) #u(s))

(defdimen length? (qty) #u(m))

(defdimen mass? (qty) #u(g))

(defdimen luminosity? (qty) #u(candela))

(defdimen temperature? (qty) #u(kelvin))

(defdimen electric-current? (qty) #u(ampere))

(defdimen amount-of-substance? (qty) #u(mole))

;;; derived dimensions
(defdimen force? (qty) #u(N))
(defdimen weight? (qty) #u(N)) ;; redundant, I know...

(defdimen energy? (qty) #u(J))

(defdimen power? (qty) #u(W))

(defdimen velocity? (qty) #u(m / s))

(defdimen acceleration? (qty) #u(m / s ^ 2))
