(in-package #:dpans-printer.single-file)

(defun print-spec-stream ()
  (let ((s (make-string-output-stream)))
    (print-element s (parse-spec))
    (get-output-stream-string s)))

(defun make-indent (indent)
  (if (= indent 0) ""
      (concatenate 'string " " (make-indent (- indent 1)))))

(defun print-children (stream element indent)
  (when (> (length (children element)) 0)
    (loop with new-indent = (+ 2 indent)
          for child in (children element)
          do (print-element stream child new-indent))))

(defgeneric print-element (s element &optional indent))

(defmethod print-element (s (element <element>) &optional (indent 0))
  (declare (ignore s indent))
  (error "No proper printer was defined for ~a" element))

(defmethod print-element (s (element <document>) &optional (indent 0))
  (declare (ignore indent))
  (format s "~:
<DOCTYPE! html>
<html>
  <head>
    <title>Common lisp spec</title>
  </head>
  <body>
")
  (print-children s element 2)
  (format s "~:
  </body>
</html>"))

(defmethod print-element (s (element <container-block-element>) &optional (indent 0))
  (let* ((tag-name (name element))
         (tag (cond
                ((string= tag-name "section") "h2")
                ((string= tag-name "subsection") "h3")
                ((string= tag-name "subsubsection") "h4")
                ((string= tag-name "subsubsubsection") "h5")
                ((string= tag-name "subsubsubsubsection") "h6")
                ))
         (pad (make-indent indent)))
    (format s "~:
~a<div class=~s>
~a  <~a>~a</~a>
" pad tag-name pad tag (title element) tag)
    (print-children s element indent)
    (format s "~:
~a</div>
" pad)))


(defmethod print-element (stream (element <displaythree>) &optional (indent 0))
  (let ((pad (make-indent indent)))
    (format stream "~:
~a<div class=\"display-table--three\">
~a  <div class=\"dispay-table__title\">~a</div>
~a  <ul class=\"dispay-table__list\">
"
            pad pad (title element) pad)
    (print-children stream element indent)
    (format stream "~:
~a  </ul>
~a</div>
"
            pad pad)))

(defmethod print-element (stream (element <displaythree-func>) &optional (indent 0))
  (let ((pad (make-indent indent)))
    (format stream "~a<li class=\"display-table__list__item\">~a</li>~%"
            pad (name element))))

(defmethod print-element (stream (element <paragraph>) &optional (indent 0))
  (let ((pad (make-indent indent)))
    (format stream "~a<p>" pad)
    (print-children stream element indent)
    (format stream "</p>~%")))

(defmethod print-element (stream (element string) &optional (indent 0))
  (declare (ignore indent))
  (format stream "~a" element))

(defmethod print-element (stream (element <term>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<a class=\"link--term\" href=~s>~a</a>" (term element) (text element)))

(defmethod print-element (stream (element <new-term>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<a class=\"link--new-term\" href=~s>~a</a>" (term element) (text element)))

(defmethod print-element (stream (element <seevar>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<a class=\"link--var\" href=~s>~a</a>" (name element) (name element)))

(defmethod print-element (stream (element <seefuns>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<a class=\"link--fun\" href=~s>~a</a>" (name element) (name element)))

(defmethod print-element (stream (element <funref>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<a class=\"link--fun\" href=~s>~a</a>" (name element) (name element)))

(defmethod print-element (stream (element <varref>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<a class=\"link--var\" href=~s>~a</a>" (name element) (name element)))

(defmethod print-element (stream (element <typeref>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<a class=\"link--type\" href=~s>~a</a>" (name element) (name element)))

(defmethod print-element (stream (element <chapref>) &optional (indent 0))
  (declare (ignore indent))
  (let ((name (name element)))
    (format stream "<a class=\"link--section\" href=~s>~a</a>"
            (name element)
            (or (gethash name (props (document element)))
                "I DO NOT EXIST AND WILL THROW ERROR IN THE FUTURE"))))

(defmethod print-element (stream (element <metavar>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<i>~a</i>" (name element)))

(defmethod print-element (stream (element <formula>) &optional (indent 0))
  (format stream "<formula>")
  (print-children stream element indent)
  (format stream "</formula>"))

(defmethod print-element (stream (element <formula-sub>) &optional (indent 0))
  (format stream "<sub>")
  (print-children stream element indent)
  (format stream "</sub>"))

(defmethod print-element (stream (element <formula-symbol>) &optional (indent 0))
  (declare (ignore indent))
  (format stream "<symbol>~a</symbol>" (name element)))
