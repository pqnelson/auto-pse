(defsystem "auto-pse"
  :version "0.1.0"
  :author "Alex Nelson"
  :license "GPL3 License"
  :depends-on ("physical-quantities"
               "gsll") ;; temporary (hopefully)
  :components ((:module "src"
                :components
                ((:file "match")
                 (:file "simplify" :depends-on ("match"))
                 (:module "math"
                  :components
                  ((:file "root-finding")))
                 (:module "physics"
                  :components
                  ((:file "utils")
                   (:file "constants")
                   (:file "thermodynamics" :depends-on ("utils" "constants"))))
                 (:file "main"))))
  :description "A problem solver environment for scientific endeavors."
  :in-order-to ((test-op (test-op "auto-pse/tests"))))

(defsystem "auto-pse/tests"
  :author "Alex Nelson"
  :license "GPL3 License"
  :depends-on ("auto-pse"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "simplify-tests")
                 (:file "match-tests")
                 (:module "math"
                  :components
                  ((:file "root-finding-tests")))
                 (:module "physics"
                  :components
                  ((:file "thermodynamics-tests")))
                 (:file "main"))))
  :description "Test system for auto-pse"
  :perform (test-op (op c) (symbol-call :rove :run c)))
