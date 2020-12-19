(defpackage #:auto-pse.physics.constants
  (:use #:cl pq)
  (:export speed-of-light h-bar k-boltzmann stefan-boltzmann planck
           avogadro universal-gas gravitational))
(in-package #:auto-pse.physics.constants)

;; TODO: figure out a smarter way if the SI units have been defined
(unless (pq::has-key "yocto" pq::*unit-prefix-table*)
  (define-si-units))
(define-read-macros)

(defmacro def-physical-constant (name val)
  `(progn
     (export ',(intern (string name) 'auto-pse.physics.constants)
             'auto-pse.physics.constants)
     (unless (boundp ',name)
       (defconstant ,name ,val))))

(def-physical-constant speed-of-light #q(299792458 m / s))
(def-physical-constant planck #q((* 662607015 (expt 10 -42)) J s))
(def-physical-constant h-bar (q/ planck-constant (* 2d0 pi)))
(def-physical-constant k-boltzmann #q((* 1380649 (expt 10 -29)) J / K))
(def-physical-constant stefan-boltzmann ;; CODATA 2018 definition
    (q* (expt pi 5) (q/ (q* 2/15 (qpow k-boltzmann 4))
                        (q* planck-constant
                            (qpow (q* planck-constant speed-of-light) 2)))))

(def-physical-constant avogadro #q((* 602214076 (expt 10 15)) / mole))
(def-physical-constant universal-gas (q* avogadro-const k-boltzmann))

(def-physical-constant gravitational
  #q(6.7430d-11 +/- 1.5d-16 m ^ 3 s ^ -2 kg ^ -1))

