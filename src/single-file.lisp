(in-package #:dpans-printer.single-file)

(defun print-spec-stream ()
  (print-element (parse-spec)))

(defgeneric print-element (element))

(defmethod print-element ((element <element>))
  (error "No proper printer was defined for ~a" element))

(defmethod print-element ((element <document>))
  (setf (html-mode) :html5)
  (with-html-output-to-string (stream nil :prologue t :indent t)
    (:html (:body
                  "Not much there"))))
