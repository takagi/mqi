(defsystem "mqi"
  :version "0.1.0"
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on (:mito)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "sxql")
                 (:file "relation")
                 (:file "relation-to-sql")
                 (:file "query")
                 (:file "util")
                 (:file "mqi"))))
  :description "Mito Querying Interface"
  :in-order-to ((test-op (test-op "mqi/tests"))))

(defsystem "mqi/tests"
  :author "Masayuki Takagi"
  :license "MIT"
  :depends-on ("mqi"
               "prove")
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:test-file "relation")
                 (:test-file "relation-to-sql")
                 (:test-file "mqi"))))
  :description "Test system for mqi"
  :perform (test-op (op c)
             (symbol-call :prove :run c)
             #+nil
             (let ((prove:*enable-colors* nil))
               (symbol-call :prove :run c))))
