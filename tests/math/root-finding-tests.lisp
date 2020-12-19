(defpackage #:auto-pse.math.root-finding-tests
  (:use #:cl #:rove #:auto-pse.math.root-finding))
(in-package #:auto-pse.math.root-finding-tests)

(deftest burden-faires-ch2-ex9b-bisection
  (labels ((f (x)
             (- (- (exp x) 2d0)
                (cos (- (exp x) 2d0)))))
    (let ((root (bisect #'f 0.5d0 1.5d0)))
      (ok (< (abs (funcall #'f root)) 1d-11)))))

(deftest burden-faires-ch2-ex5a-bisection
  (labels ((a (x)
             (- x (expt 2 (- x)))))
    (let ((root (bisect #'a 0d0 1d0)))
      (ok (< (abs (funcall #'a root)) 1d-11)))))

(deftest burden-faires-ch2-ex5b-bisection
    (labels ((b (x)
               (+ (exp x)
                  (- (* x x))
                  (* 3 x)
                  -2)))
      (let ((root (bisect #'b 0d0 1d0)))
        (ok (< (abs (funcall #'b root)) 1d-11)))))

(deftest burden-faires-ch2-ex5c-bisection
    (labels ((c (x)
               (- (* 2 x (cos (* 2 x)))
                (expt (1+ x) 2))))
      (let ((root (bisect #'c -3d0 -2d0)))
        (ok (< (abs (funcall #'c root)) 1d-11)))))

(deftest burden-faires-ch2-ex5d-bisection
    (labels ((d (x)
               (+ (* x (cos x))
                  (* -2 x x)
                  (* 3 x)
                  -1)))
      (let ((root (bisect #'d 0.2d0 0.3d0)))
        (ok (< (abs (funcall #'d root)) 1d-11)))))

(deftest burden-faires-ch2-ex6a-bisection
    (labels ((a (x)
               (- (* 3 x) (exp x))))
    (let ((root (bisect #'a 1d0 2d0)))
      (ok (< (abs (funcall #'a root)) 1d-11)))))

(deftest burden-faires-ch2-ex6b-bisection
    (labels ((b (x)
               (- (+ x (* 3 (cos x)))
                  (exp x))))
      (let ((root (bisect #'b 0d0 1d0)))
        (ok (< (abs (funcall #'b root)) 1d-11)))))

(deftest burden-faires-ch2-ex6c-bisection
    (labels ((c (x)
               (- (expt (- x 2) 2)
                  (log x))))
      (let ((root (bisect #'c 1d0 2d0)))
        (ok (< (abs (funcall #'c root)) 1d-11)))))

(deftest burden-faires-ch2-ex6d-bisection
    (labels ((d (x)
               (- (1+ x)
                  (* 2 (sin (* pi x))))))
      (let ((root (bisect #'d 0d0 0.5d0)))
        (ok (< (abs (funcall #'d root)) 1d-11)))))
