(in-package :cl-user)
(defpackage dpans-printer-test-asd
  (:use :cl :asdf))
(in-package :dpans-printer-test-asd)

(asdf:defsystem #:dpans-printer-test
  :author "Dmitry Petrov"
  :license "Public Domain"
  :depends-on (:dpans-printer
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "printer"))))
  :description "Test system for dpans-printer"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

