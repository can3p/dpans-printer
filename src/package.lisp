;;;; package.lisp

(defpackage #:dpans-printer
  (:use #:cl #:dpans-parser)
  (:export #:print-spec-to-file))

(defpackage #:dpans-printer.single-file
  (:use #:cl #:dpans-parser #:cl-xml-gen))

