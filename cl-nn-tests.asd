(asdf:defsystem #:cl-nn-tests
  :pathname "t/"
  :serial t
  :depends-on (:cl-nn :fiveam)
  :components ((:file "package")
               (:file "tests")))
