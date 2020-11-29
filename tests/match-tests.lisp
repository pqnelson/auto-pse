(defpackage #:auto-pse.match-tests
  (:use #:cl #:rove #:auto-pse.match))
(in-package #:auto-pse.match-tests)

(deftest match-element-test
  ;; regression test, grr
  (ok (fail? (auto-pse.match::match-element '(? x numberp) 'a '((A1 (A B 3 C) (A B 3 C)))))))

(deftest match-with-predicate-tests
  ;; another regression test
  (ok (equalp (match '(+ (?? A1) (? X NUMBERP) (?? a2)) '(+ a b 3 c d e))
              '((A2 (C D E) NIL)
                (X 3)
                (A1 (A B 3 C D E) (3 C D E))))))

