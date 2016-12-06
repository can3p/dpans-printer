;;;; dpANS-printer.lisp

(in-package #:dpans-printer)

(defun print-spec ()
  (dpans-printer.single-file::print-spec-stream))

(defun print-spec-to-file ()
  (ensure-directories-exist #p"build/")
  (let ((fname (make-pathname :directory '(:relative "build")
                              :name "index" :type "html")))
    (with-open-file (s fname :direction :output :if-exists :supersede)
      (format s (print-spec)))))
