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
(defpackage #:auto-pse.simplify-tests
  (:use #:cl #:rove #:auto-pse.simplify))
(in-package #:auto-pse.simplify-tests)

(deftest test-algebra-rules
  (testing "simplify zeroes"
    (ok (zerop (let ((auto-pse.simplify::*cached?* nil))
                 (simplify '(* a b 0 c d)
                           auto-pse.simplify::*algebra-rules*))))
    (ok (equal (let ((auto-pse.simplify::*cached?* nil))
                 (simplify '(+ a b 0 c d)
                           auto-pse.simplify::*algebra-rules*))
               '(+ a b c d)))
    (ok (equal (let ((auto-pse.simplify::*cached?* nil))
                 (simplify '(+ a 0 b 0 c 0 d 0)
                           auto-pse.simplify::*algebra-rules*))
               '(+ a b c d))))
  (testing "arithmetic"
    (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                  (simplify '(+ a b 3 c d 4 x y)
                            auto-pse.simplify::*algebra-rules*))
                '(+ 7 a b c d x y)))
    
    (ok (= (let ((auto-pse.simplify::*cached?* nil))
             (simplify '(+ 5 3 4)
                       auto-pse.simplify::*algebra-rules*))
           12))
    
    (ok (= (let ((auto-pse.simplify::*cached?* nil))
             (simplify '(- 5 3 4)
                       auto-pse.simplify::*algebra-rules*))
           -2))))



;; expt-simplify-rules should not simplify the exponent, *algebra-rules*
;; will be responsible for that
(deftest test-expt-simplify-rules
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(expt 1 (* a b c))
                          auto-pse.simplify::*expt-simplify-rules*))
              '1))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(expt (* a b c) 1)
                          auto-pse.simplify::*expt-simplify-rules*))
              '(* a b c)))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(* (expt a (- 4)) b (expt a 4))
                          auto-pse.simplify::*expt-simplify-rules*))
              '(* b)))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(* (expt a -2) b (expt a 4))
                          auto-pse.simplify::*expt-simplify-rules*))
              '(* b (expt a (+ -2 4)))))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(* a b (expt a 4))
                          auto-pse.simplify::*expt-simplify-rules*))
              '(* b (expt a (+ 4 1)))))
  (testing "form expt"
    (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                  (simplify '(* a b b d)
                            auto-pse.simplify::*expt-simplify-rules*))
                '(* a (expt b 2) d)))
    (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                  (simplify '(* a b c b d)
                            auto-pse.simplify::*expt-simplify-rules*))
                '(* a b c b d))))
  (testing "divide expt by var cases"
    (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                  (simplify '(* a (expt x m) b (/ c (* g x)) d)
                            auto-pse.simplify::*expt-simplify-rules*))
                '(* A (EXPT X (- M 1)) B (/ C (* G)) D)))
    (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                  (simplify '(* a (expt x m) b (/ c (* g (expt x n))) d)
                            auto-pse.simplify::*expt-simplify-rules*))
                '(* A (EXPT X (- M N)) B (/ C (* G 1)) D)))
    (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                  (simplify '(* a (expt x m) b (/ c (* g (expt (* h x k) n))) d)
                            auto-pse.simplify::*expt-simplify-rules*))
                '(* A (EXPT X (- M N)) B (/ C (* G (EXPT (* 1 H K) N))) D)))
    ))

(deftest test-double-negation-rules
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(- (- (* a b c)))
                          auto-pse.simplify::*double-negation-rules*))
              '(* A B C)))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(* x1 (- (* a b c)) x2 -1 x3)
                          auto-pse.simplify::*double-negation-rules*))
              '(* X1 (* A B C) X2 X3)))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(* x1 -1 x2 (- (* a b c)) x3)
                          auto-pse.simplify::*double-negation-rules*))
              '(* X1 X2 (* A B C) X3)))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(* x1 (- (/ (* d e) f)) x2 (- (* a b c)) x3)
                          auto-pse.simplify::*double-negation-rules*))
              '(* X1 (/ (* D E) F) X2 (* A B C) X3))))

(deftest test-log-expand
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(log (* x y))
                          auto-pse.simplify::*log-expand*))
              '(+ (LOG X) (LOG (* Y)))))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(log (* w x y z))
                          auto-pse.simplify::*log-expand*))
              '(+ (LOG W) (+ (LOG X) (+ (LOG Y) (LOG (* Z)))))))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(log (/ x y))
                          auto-pse.simplify::*log-expand*))
              '(- (LOG X) (LOG Y))))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(log (expt x y))
                          auto-pse.simplify::*log-expand*))
              '(* Y (LOG X)))))

(deftest test-sqrt-expand
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(sqrt (* a b))
                          auto-pse.simplify::*sqrt-expand*))
              '(* (sqrt a) (sqrt b))))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(sqrt (* a b c))
                          auto-pse.simplify::*sqrt-expand*))
              '(* (sqrt a) (* (sqrt b) (sqrt c)))))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(sqrt (/ a b))
                          auto-pse.simplify::*sqrt-expand*))
              '(/ (sqrt a) (sqrt b))))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(sqrt (/ a b c))
                          auto-pse.simplify::*sqrt-expand*))
              '(/ (sqrt a) (* (sqrt b) (sqrt c))))))


(deftest test-obvious-ones
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(+ a (expt (sin x) 42) b (expt (cos x) 42) c)
                          auto-pse.simplify::*obvious-ones*))
              '(+ 1 a b c)))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(+ a (expt (sin x) 4) b (expt (cos x) 4) c)
                          auto-pse.simplify::*obvious-ones*))
              '(+ 1 a b c)))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(+ a (expt (cos x) 4) b (expt (sin x) 4) c)
                          auto-pse.simplify::*obvious-ones*))
              '(+ 1 a b c))))
