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

