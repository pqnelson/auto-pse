(defpackage #:auto-pse.simplify-tests
  (:use #:cl #:rove #:auto-pse.simplify))
(in-package #:auto-pse.simplify-tests)

(deftest test-simplify-for-zeroes
  (ok (zerop (let ((auto-pse.simplify::*cached?* nil))
               (simplify '(* a b 0 c d)))))
  (ok (equal (let ((auto-pse.simplify::*cached?* nil))
               (simplify '(+ a b 0 c d)))
             '(+ a b c d)))
  (ok (equal (let ((auto-pse.simplify::*cached?* nil))
               (simplify '(+ a 0 b 0 c 0 d 0)))
             '(+ a b c d))))

(deftest simplify-with-arithmetic
  (ok (equal (let ((auto-pse.simplify::*cached?* nil))
               (simplify '(+ a b 3 c d 4 x y)))
             '(+ a b c d x y 7)))

  (ok (= (let ((auto-pse.simplify::*cached?* nil))
           (simplify '(+ 5 3 4)))
         12))
  
  (ok (= (let ((auto-pse.simplify::*cached?* nil))
           (simplify '(- 5 3 4)))
         -2)))

(deftest test-expt-simplify-rules
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(* (expt a (- 4)) b (expt a 4))
                          auto-pse.simplify::*expt-simplify-rules*))
              '(* b)))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(* (expt a -2) b (expt a 4))
                          auto-pse.simplify::*expt-simplify-rules*))
              '(* b (expt a 2))))
  (ok (equalp (let ((auto-pse.simplify::*cached?* nil))
                (simplify '(* a b (expt a 4))
                          auto-pse.simplify::*expt-simplify-rules*))
              '(* b (expt a 5)))))
