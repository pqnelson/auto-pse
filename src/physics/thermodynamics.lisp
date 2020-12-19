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
(defpackage #:auto-pse.physics.thermodynamics
  (:use #:cl #:auto-pse.physics.utils pq)
  (:local-nicknames (#:constant #:auto-pse.physics.constants))
  (:export black-body-radiation-power irradiance)
  (:documentation "Code Useful for thermodynamic and stat. mech. calculations."))
(in-package #:auto-pse.physics.thermodynamics)

(defun black-body-radiation-power (r temperature)
  "Using Stefan-Boltzmann law to estimate the power radiated of by a blackbody of given radius R and TEMPERATURE."
  (assert (length? r))
  (assert (temperature? temperature))
  (convert-unit
   (q* 4d0 pi
       constant:stefan-boltzmann
       (qpow r 2)
       (qpow temperature 4))
   #u(J / s)))

(defun ex0 ()
  (let ((sun-temperature     #q(5772 K))
        (sun-radius        #q(695700 km)))
    (black-body-radiation-power sun-radius sun-temperature)))

(defun irradiance (power distance)
  (assert (length? distance))
  (assert (power? power))
  ;; = power / (4 pi R^2)
  (convert-unit
   (q* (q/ power pi)
       (qpow (q* 2 distance) -2))
   #u(W / m ^ 2)))

(defun ex1 ()
  (let ((sun-temperature     #q(5772 K))
        (sun-radius        #q(695700 km))
        (earth-distance #q(149596000 km)))
     (irradiance
      (black-body-radiation-power sun-radius sun-temperature)
      earth-distance)))

(defun ex2 ()
  (let ((sun-temperature     #q(5772 K))
        (sun-radius        #q(695700 km))
        (sun-luminosity  #q(3.828e26 W))
        (earth-distance #q(149596000 km)))
    (convert-unit
     (irradiance sun-luminosity earth-distance)
     #u(W / m ^ 2))))
