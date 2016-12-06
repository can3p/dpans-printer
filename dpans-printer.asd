;;;; dpANS-printer.asd

(in-package :cl-user)
(defpackage dpans-printer-asd
  (:use :cl :asdf))
(in-package :dpans-printer-asd)

(asdf:defsystem #:dpans-printer
  :description "Spec printer that consumes output from dpANS-parser"
  :author "Dmitry Petrov <dpetroff@gmail.com>"
  :license "Public Domain"
  :depends-on (#:dpans-parser
               #:cl-who)
  :serial t
  :components ((:module "src"
                :components
                (
                 (:file "package")
                 (:file "single-file")
                 (:file "dpans-printer")
                 )))
  :in-order-to ((test-op (test-op dpans-printer-test))))

