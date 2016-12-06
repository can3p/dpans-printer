(in-package :cl-user)
(defpackage dpans-printer-test
  (:use :cl
   :dpans-printer
        :prove))

(in-package :dpans-printer-test)

;; NOTE: To run this test file, execute `(asdf:test-system :dpans-printer)' in your Lisp.

(plan nil)

(is 1 1)

(finalize)

