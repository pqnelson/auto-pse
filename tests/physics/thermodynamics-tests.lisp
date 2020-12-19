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
(defpackage #:auto-pse.physics.thermodynamics-tests
  (:use #:cl #:rove #:auto-pse.physics.thermodynamics pq))
(in-package #:auto-pse.physics.thermodynamics-tests)

(deftest black-body-ration-power-test
  (let ((sun-temperature     #q(5772 K))
        (sun-radius        #q(695700 km)))
    (ok (< (abs
            (pq:value
             (q/ (q- (black-body-radiation-power sun-radius sun-temperature)
                     #q(3.82799090d26 J / s))
                 #q(3.82799090d26 J / s))))
           1d-7))))

(deftest irradiance-test
  (let ((sun-luminosity  #q(3.828e26 W))
        (earth-distance #q(149596000 km)))
    (ok (< (abs
            (pq:value
             (q/ (q- (irradiance sun-luminosity earth-distance)
                     #q(1361.2 W / m ^ 2))
                 #q(1361.2 W / m ^ 2))))
           1d-6))))
