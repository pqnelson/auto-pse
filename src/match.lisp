(defpackage #:auto-pse.match
  (:use #:cl)
  (:export match substitute-in fail? ? ??)
  (:documentation "Match data against a pattern.

"))
(in-package #:auto-pse.match)

;; I've had issues where rules failed because '? was interned in the wrong
;; package, so to avoid this, I'm just exporting '? and '?? for all uses.
(defparameter ? '?)
(defparameter ?? '??)
(defun fail? (x) (eq x :fail))

;; TODO: handle reverse segment?
(defun match (pattern data &optional (dict nil))
  "Produce the bindings from matching the PATTERN against the DATA sexp.

If given a DICT, try to use the existing bindings (if the variables are
already bound) or extend the DICT with new bindings. Otherwise, creates a new
alist."
  (cond
    ((fail? dict) :fail)

    ((eq pattern data)
     dict)

    ((pattern-element? pattern)
     (match-element pattern data dict))

    ((not (consp pattern))
     (if (equal? pattern data)
         dict
         :fail))

    ((pattern-segment? (car pattern))
     (match-segment pattern data dict))

    ((not (consp data))
     :fail)
    
    (t (let ((new-bindings (match (car pattern) (car data) dict)))
         (if (fail? new-bindings)
             :fail
             (match (cdr pattern) (cdr data) new-bindings))))
    ))

(defun equal? (lhs rhs)
  (or (and (numberp lhs) (numberp rhs) (= lhs rhs))
      (equalp lhs rhs)))

(defun var-name (pattern-var)
  "Both pattern variables and segment variables look like (? X) or (?? X).
This returns that `X'."
  (assert (variable? pattern-var))
  (cadr pattern-var))

(defun var-restriction (pattern)
  "When we want to restrict the pattern using a predicate, this returns the predicate.

Example: (var-restriction '(? n numberp)) => #'numberp."
  (assert (variable? pattern))
  (caddr pattern))

(defun lookup-var (pattern dict)
  "Return whatever value we bound to the PATTERN in the DICT."
  (assert (variable? pattern))
  (assoc (var-name pattern) dict))

(defun match-element (pattern data dict &aux entry pred)
  "Try matching a PATTERN (? x ...) to the DATA sexp using the existing DICT
bindings. If it's not possible, returns :FAIL. Otherwise, return the extended
DICT."
  (assert (pattern-element? pattern))
  (setq entry (lookup-var pattern dict))
  (cond
    (entry
     (if (equal? (cadr entry) data)
         dict
         :fail))
    (t (setq pred (var-restriction pattern))
       (cond
         ((or (not pred)
              (funcall pred data))
          (bind-var (var-name pattern) data dict))
         (t :fail)))))

(defun match-segment (pattern data dict &aux entry pred end rest)
  "Attempts to match a segment PATTERN against an sexp DATA using the existing
DICT. If a new match is found, the DICT is extended. If an existing binding
works, the DICT is returned 'as is'. Otherwise, we have :FAIL-ed."
  ;; (assert (pattern-segment? (car pattern)))
  (setq entry (lookup-var (car pattern) dict))
  (cond
    (entry (setq rest
                 (check-segment data
                                (segment-begin entry)
                                (segment-end entry)))
           (if (fail? rest)
               :fail
               (match (cdr pattern) rest dict)))
    (t ;; searching for new bindings
     (try-segment-bindings (car pattern) (cdr pattern) data dict))))

;; Segment entries are of the form (<name> <begin> <end>).
(defun segment-begin (entry)
  (cadr entry))

(defun segment-end (entry)
  (caddr entry))

(defun check-segment (data begin end)
  "Given the segment variable already bound to some value, check that DATA
matches the segment between BEGIN and END. If not, we have an epic :FAIL."
  (cond ((eq begin end) data)
        ((not (consp data)) :fail)
        ((equal? (car data) (car begin))
         (check-segment (cdr data) (cdr begin) end))
        (t :fail)))

(defun try-segment-bindings (var pattern data dict &aux name pred begin)
  "We have the segment VAR followed by the rest of the PATTERN matching the
DATA, extending the DICT upon success. Otherwise we return :FAIL."
  (assert (pattern-segment? var))
  (setq name (var-name var)
        pred (var-restriction var)
        begin data)
  (do ((end data (cdr end))
       (ndict nil))
      ((null end)
       (cond ((or (null pred)
                  (funcall pred (segment->list begin nil)))
              (match pattern
                nil
                (bind-segment name
                              begin nil ;; (make-segment begin nil)
                              dict)))
             (t :fail)))
    (when (or (null pred)
              (funcall pred (segment->list begin end)))
      (setq ndict (match pattern end
                    (bind-segment name begin end dict)))
      (unless (fail? ndict)
        (return-from try-segment-bindings ndict)))))

(defun segment->list (begin end)
  "Given the BEGIN and END parts of the dictionary entry for a segment, return
the value captured for the segment variable."
  (do ((point begin (cdr point))
       (l nil))
      ((eq point end) 
       (nreverse l))
    (push (car point) l)))

(defun bind-var (variable data dictionary)
  "Bind the pattern element VARIABLE to value DATA in the given DICTIONARY."
  (let ((new-entry (list variable data)))
    (cons new-entry dictionary)))

(defun bind-segment (variable begin end dictionary)
  "Bind the segment VARIABLE from BEGIN to END in the given DICTIONARY."
  (let ((new-entry (list variable begin end)))
    (cons new-entry dictionary)))

(defun variable? (pattern)
  "A pattern variable is either an element or a segment."
  (or (pattern-element? pattern)
      (pattern-segment? pattern)))

(defun pattern-element? (pattern)
  "A pattern element is a variable of the form (? x) or (? x predicate-p).

An element matches one value."
  (and (consp pattern)
       (eq (car pattern) ?)
       (null (cdddr pattern))))

(defun pattern-segment? (pattern)
  "A segment looks like (?? x) or (?? x predicate-p).

A segment matches multiple values."
  (and (consp pattern)
       (eq (car pattern) ??)
       (null (cdddr pattern))))

(defun var-value (variable dict &aux (entry))
  "Compute the value bound to the pattern or segment VARIABLE in the DICT.

VARIABLE is either a segment-variable or a pattern-variable, without any
restriction. Used to splice the associated bound value into a larger
expression."
  (assert (variable? variable))
  (setq entry (lookup-var variable dict))
  (unless entry
    (error "Not bound variable: ~A, ~A." variable dict))
  (cond ((= (length entry) 2) (cadr entry))
        (t (segment->list (segment-begin entry) (segment-end entry)))))

(defun substitute-in (expr dict)
  "Given a pattern EXPR and a DICT of bindings, substitute the DICT values for
associated pattern variables."
  (cond ((null expr) nil)
        
        ((pattern-element? expr)
         (var-value expr dict))

        ((consp expr)
         (cond ;; expr ~ ((?? x) e2)
               ((pattern-segment? (car expr))
                (append (var-value (car expr) dict)
                        (substitute-in (cdr expr) dict)))
               ;; expr ~ (:EVAL (stuff))
               ((eq (car expr) :eval)
                (eval (substitute-in (cadr expr) dict)))
               ;; expr ~ ((:SPLICE (stuff)) remaining-expr)
               ((and (consp (car expr))
                     (eq (caar expr) :splice))
                (append (substitute-in (cadar expr) dict)
                        (substitute-in (cdr expr) dict)))
               ;; expr ~ (e1 ...)
               (t (cons (substitute-in (car expr) dict)
                        (substitute-in (cdr expr) dict)))))
        ;; fall through
        (t expr)))
