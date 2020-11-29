(defpackage auto-pse/tests/main
  (:use :cl
        :auto-pse
        :rove))
(in-package :auto-pse/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :auto-pse)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
