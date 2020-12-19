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
(defpackage auto-pse
  (:use :cl))
(in-package :auto-pse)

;; (defconstant program-name "auto-pse")
(unless (boundp '+program-name+)
  (defconstant +program-name+ "auto-pse"))
(unless (boundp '+authors+)
  (defconstant +authors+ "Alex Nelson"))
(unless (boundp '+copyright+)
  (defconstant +copyright+ "(C) 2020"))

(defun show-warranty ()
  (format t "~%   THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY")
  (format t "~% APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT")
  (format t "~% HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY")
  (format t "~% OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,")
  (format t "~% THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR")
  (format t "~% PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM")
  (format t "~% IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF")
  (format t "~% ALL NECESSARY SERVICING, REPAIR OR CORRECTION."))

(defun show-conditions ()
  (format t "~% You should have received a copy of the GNU General Public License")
  (format t "~% along with this program.  If not, see <https://www.gnu.org/licenses/>."))

(defun splash-message ()
  (format t "~% ~A  Copyright ~A  ~A" +program-name+ +copyright+ +authors+)
  (format t "~% This program comes with ABSOLUTELY NO WARRANTY;")
  (format t "~% for details type `show-warranty'.")
  (format t "~% ")
  (format t "~% This is free software, and you are welcome to redistribute it")
  (format t "~% under certain conditions; type `show-conditions' for details.")
  (format t "~% "))


;; blah blah blah.
