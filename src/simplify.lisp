(defpackage #:auto-pse.simplify
  (:use #:cl #:auto-pse.match)
  (:export simplify)
  (:documentation "Simplify mathematical expressions.

Inspired from Gerald Sussman's SCMUTILS, especially the `simplify/rules.scm'
code."))
(in-package #:auto-pse.simplify)

(defparameter *simplify-cache* (make-hash-table :TEST #'equal))
(defparameter *cached?* nil)
(defparameter *debug?* nil)

(defun simplify (expr &optional (rules *algebra-rules*))
  (if *cached?*
      (or (gethash expr *simplify-cache*)
          (let ((result (simplify* expr rules)))
            (setf (gethash expr *simplify-cache*) result)
            result))
      (simplify* expr rules)))

(defun simplify* (expr rules &aux result)
  (setq result
        (try-matcher-rules (if (listp expr)
                               (mapcar #'simplify expr)
                               expr)
                           rules))
  (if (equal result expr)
      result
      (simplify* result rules)))

(defun try-matcher-rules (expr rules)
  "Try to simplify EXPR using each rule in RULES."
  (dolist (rule rules expr)
    (when *debug?*
      (format t "~% Trying to match: ~A" rule))
    (let ((bindings (match (rule-pattern rule) expr nil)))
      (when (satisfies-rule-predicate? rule bindings)
        (progn
          (when *debug?*
            (unless (fail? bindings)
              (format t "~% Matched ~A on ~A." expr rule)
              (dolist (binding bindings)
                (format t "~%   ~A: ~A." (car binding)
                        bindings))))
          (unless (fail? bindings)
            ;; returns the first match
            (return-from try-matcher-rules
              (substitute-in (rule-result rule)
                             bindings))))
        ;; else, (fail? bindings) iterates the loop
        ))))

;; Following SCMUTILS, a rule for simplification consists of three components
;; (1) the pattern, (2) the results, (3) the condition of application.
;; The first two are familiar from usual pattern matching.
;; 
;; But sometimes, we want to "turn on" (or "turn off") a simplification
;; rule, which is handled by the third parameter. The `rule-condition'
;; could be a boolean parameter, or it could be a function. If there is no
;; third parameter, the rule applies by default.
(defun rule-pattern (rule) (car rule))
(defun rule-result (rule) (cadr rule))

(defun satisfies-rule-predicate? (rule bindings)
  "When the RULE has some condition, check that the BINDINGS satisfy the
restriction."
  (let* ((end-of-rule (cddr rule)))
    (or (null end-of-rule)
        ;; else END-OF-RULE is '(RULE-CONDITION)
        (progn
          ;; raise an error if END-OF-RULE is something else, for some reason
          (assert (consp end-of-rule))
          (let ((rule-condition (car end-of-rule)))
            (or
             (when (typep rule-condition 'boolean)
               rule-condition)
             (when (functionp rule-condition)
               (funcall rule-condition))
             ;; error?
             ))
          ))))
;; (satisfies-rule-predicate? '(1 2))
;; (not (satisfies-rule-predicate? '(1 2 nil)))

(setq *algebra-rules*
      '(((+ (?? x) 0 (?? y))
         (+ (?? x) (?? y)))
        ((- (?? x) 0 (?? y))
         (- (?? x) (?? y)))
        ((* (?? x) 1 (?? y))
         (* (?? x) (?? y)))
        ((* (?? x) 0 (?? y))
         0)
        ((* (?? a1) (? x numberp) (?? a2) (? y numberp) (?? a3))
         (* (?? a1) (?? a2) (?? a3) (:eval (* (? x) (? y)))))
        ;; TODO: handle (?? a1) = (?? a2) = (?? a3) = nil
        ((+ (?? a1) (? x numberp) (?? a2) (? y numberp) (?? a3))
         (+ (?? a1) (?? a2) (?? a3) (:eval (+ (? x) (? y)))))
        ((- (?? a1 consp) (? x numberp) (?? a2) (? y numberp) (?? a3))
         (- (?? a1) (?? a2) (?? a3) (:eval (+ (? x) (? y)))))
        ((- (? x numberp) (?? a1 consp) (? y numberp) (?? a2))
         (- (:eval (- (? x) (? y))) (?? a1) (?? a2)))
        ((- (? x numberp) (?? a1) (? y numberp) (?? a2 consp))
         (- (:eval (- (? x) (? y))) (?? a1) (?? a2)))
        ((- (? x numberp) (? y numberp))
         (:eval (- (? x) (? y))))
        ((* (? x))
         (? x))
        ((+ (? x))
         (? x))
        ((/ (?? x) 1)
         (?? x))
        ))

(defun nonzero? (x)
  (not (zerop x)))

;; TODO: the first couple rules do not work, but it'd be nice if they did
;; work. I should think about it...
(defparameter *expt-simplify-rules*
  '(((expt 1 (? x)) 1)
    ((expt (? x) 0)
     1)
    ((expt (? x) 1)
     (? x))
    ((* (?? a1) (expt (? x) (? m)) (?? a2) (expt (? x) (- (? m))) (?? a3))
     (* (?? a1) (?? a2) (?? a3)))
    ((* (?? a1) (expt (? x) (- (? m))) (?? a2) (expt (? x) (? m)) (?? a3))
     (* (?? a1) (?? a2) (?? a3)))
    ((* (?? a1) (expt (? x) (- (? m))) (?? a2) (expt (? x) (? m)) (?? a3))
     (* (?? a1) (?? a2) (?? a3)))
    ((* (?? a1) (expt (? x) (? m)) (?? a2) (expt (? x) (? n)) (?? a3))
     (* (?? a1) (?? a2) (expt (? x) (+ (? m) (? n))) (?? a3)))
    ((* (?? a1) (expt (? x) (? m)) (?? a2) (? x) (?? a3))
     (* (?? a1) (?? a2) (expt (? x) (+ (? m) 1)) (?? a3)))
    ((* (?? a1) (? x) (?? a2) (expt (? x) (? m)) (?? a3))
     (* (?? a1) (?? a2) (expt (? x) (+ (? m) 1)) (?? a3)))
    ((* (?? a1) (expt (? x) (? m)) (?? a2) (/ (?? num) (* (?? d1) (? x) (?? d2))) (?? a3))
     (* (?? a1) (expt (? x) (- (? m) 1)) (?? a2) (/ (?? num) (* (?? d1) (?? d2))) (?? a3)))
    ((* (?? a1) (expt (? x) (? m)) (?? a2) (/ (?? num) (* (?? d1) (expt (? x) (? n)) (?? d2))) (?? a3))
     (* (?? a1) (expt (? x) (- (? m) (? n))) (?? a2) (/ (?? num) (* (?? d1) 1 (?? d2))) (?? a3)))
    ((* (?? a1) (expt (? x) (? m)) (?? a2) (/ (?? num) (* (?? d1) (expt (* (?? e1) (? x) (?? e2)) (? n)) (?? d2))) (?? a3))
     (* (?? a1) (expt (? x) (- (? m) (? n))) (?? a2) (/ (?? num) (* (?? d1) (expt (* 1 (?? e1) (?? e2)) (? n)) (?? d2))) (?? a3)))
    ))

(defparameter *double-negation-rules*
  '(((- (- (? x)))
     (? x))
    ((* (?? a1) -1 (?? a2) -1 (?? a3))
      (* (?? a1) (?? a2) (?? a3)))
    ((* (?? a1) (- (? x)) (?? a2) -1 (?? a3))
     (* (?? a1) (? x) (?? a2) (?? a3)))
    ((* (?? a1) (- (? x)) (?? a2) (- (? y)) (?? a3))
     (* (?? a1) (? x) (?? a2) (? y) (?? a3)))
    ))

(defparameter sqrt-factor-simplify? nil)

(defun non-negative-factors? (x y)
  (and (not (negp (simplify x)))
       (not (negp (simplify y)))))

(defparameter sqrt-expand
  '(((sqrt (* (? x) (? y)))
     (* (sqrt (? x)) (sqrt (? y))))
    ((sqrt (* (? x) (? y) (?? ys)))
     (* (sqrt (? x)) (sqrt (* (? y) (?? ys)))))
    ((sqrt (/ (? x) (? y)))
     (/ (sqrt (? x)) (sqrt (? y))))
    ((sqrt (/ (? x) (? y) (?? ys)))
     (/ (sqrt (? x)) (sqrt (* (? y) (?? ys)))))
    ))

(defparameter *half-angle*
  '(((sin (* 1/2 (? x) (?? y)))
     
     ())
    ))



(defparameter *obvious-ones*
  '(((+ (?? a1) (expt (sin (? x)) 2) (?? a2) (expt (cos (? x)) 2) (?? a3))
     (+ 1 (?? a1) (?? a2) (?? a3)))
    ((+ (?? a1) (expt (cos (? x)) 2) (?? a2) (expt (sin (? x)) 2) (?? a3))
     (+ 1 (?? a1) (?? a2) (?? a3)))
    ))

(defun imaginary-integer? (x)
  (when (complexp x)
    (integerp (imagpart x))))

(defun even-integer? (x)
  (and (integerp x)
       (evenp x)))

(defun odd-integer? (x)
  (and (integerp x)
       (oddp x)))

(defun even-nonzero-integer? (x)
  (and (integerp x)
       (evenp x)
       (not (zerop x))))
