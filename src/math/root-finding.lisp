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
(defpackage #:auto-pse.math.root-finding
  (:use #:cl)
  (:export bisect))
(in-package #:auto-pse.math.root-finding)

(defparameter *tolerance* long-float-negative-epsilon)
(defparameter *max-steps* 53) ;; = (float-precision 1d0)
;; BUT if (float-radix 1d0) is not 2, then (float-precision 1d0) is not ~50


(defun bisect (f initial-low initial-high &key (tolerance *tolerance*) (max-steps *max-steps*) &aux low high)
  "Use bisection method to find root of F between initial high and low guesses.

Contracts:
Requires: (f initial-low) to be of opposite sign as (f initial-high).
Requires: (< initial-low initial-high).
Requires: (< 0 max-steps) and max-steps is an integer (defaults to *max-steps*)
Requires: (< 0d0 tolerance) (defaults to *tolerance*)
Ensures: (< (abs RESULT) tolerance)."
  (declare (type real initial-low initial-high tolerance)
           (type integer max-steps)
           (type (function (real) real) f))
  (assert (plusp tolerance))
  (assert (plusp max-steps))
  (assert (< initial-low initial-high))
  (setq low initial-low)
  (setq high initial-high)
  (let ((f-low (funcall f low))
        (old-p 0)
        (initial-p (* 0.5d0 (+ low high))))
    (assert (/= (signum f-low) (signum (funcall f high))))
    (do ((counter 0 (1+ counter))
         (p initial-p (* 0.5d0 (+ low high))))
        ((= counter max-steps) p)
      (let ((f-p (funcall f p)))
        (cond
          ((or (< (abs f-p) tolerance)
               (< (- high low) tolerance)
               (< (abs (- p old-p)) tolerance))
           (return-from bisect p))
          
          ((= (signum f-low) (signum f-p))
           (setq old-p p)
           (setq low p))

          (t (setq old-p p)
             (setq high p)))))))

(defun bisect-example1 ()
  "Exercise 9(b) of Ch2"
  (labels ((f (x)
             (- (- (exp x) 2d0)
                (cos (- (exp x) 2d0)))))
    (let ((root (bisect #'f 0.5d0 1.5d0)))
      (list root (funcall #'f root)))))

(defun bisect-ex2 ()
  "Exercise 5 of chapter 2"
  (labels ((a (x)
             (- x (expt 2d0 (- x))))
           (b (x)
             (+ (exp x)
                (- (* x x))
                (* 3d0 x)
                -2d0))
           (c (x)
             (- (* 2d0 x (cos (* 2d0 x)))
                (expt (1+ x) 2)))
           (d (x)
             (+ (* x (cos x))
                (* -2d0 x x)
                (* 3d0 x)
                -1d0)))
    (let ((a-root (bisect #'a 0d0 1d0))
          (b-root (bisect #'b 0d0 1d0))
          (c-root (bisect #'c -3d0 -2d0))
          (d-root (bisect #'d 0.2d0 0.3d0)))
      (list (list :a a-root (funcall #'a a-root))
            (list :b b-root (funcall #'b b-root))
            (list :c c-root (funcall #'c c-root))
            (list :d d-root (funcall #'d d-root))
            ))))

(defun bisect-6 ()
  "Exercise 6 of chapter 2"
  (labels ((a (x)
             (- (* 3 x) (exp x)))
           (b (x)
             (- (+ x (* 3 (cos x)))
                (exp x)))
           (c (x)
             (- (expt (- x 2) 2)
                (log x)))
           (d (x)
             (- (1+ x)
                (* 2 (sin (* pi x))))))
    (let ((a-root (bisect #'a 1l0 2l0))
          (b-root (bisect #'b 0l0 1l0))
          (c-root (bisect #'c 1l0 2l0))
          (d-root (bisect #'d 0l0 0.5l0)))
      (list (list :a a-root (funcall #'a a-root))
            (list :b b-root (funcall #'b b-root))
            (list :c c-root (funcall #'c c-root))
            (list :d d-root (funcall #'d d-root))))))
