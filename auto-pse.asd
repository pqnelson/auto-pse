(defsystem "auto-pse"
  :version "0.1.0"
  :author "Alex Nelson"
  :license "MIT License"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "match")
                 (:file "simplify")
                 (:file "main"))))
  :description "A problem solver environment for scientific endeavors."
  :in-order-to ((test-op (test-op "auto-pse/tests"))))

(defsystem "auto-pse/tests"
  :author "Alex Nelson"
  :license "MIT License"
  :depends-on ("auto-pse"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "simplify-tests")
                 (:file "match-tests")
                 (:file "main"))))
  :description "Test system for auto-pse"
  :perform (test-op (op c) (symbol-call :rove :run c)))
