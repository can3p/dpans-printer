(in-package #:dpans-printer.single-file)

(defun print-spec-stream ()
  (html (print-element (parse-spec))))

(defgeneric print-element (element))

(defmethod print-element ((element <element>))
  (error "No proper printer was defined for ~a" element))

(defmethod print-element ((element <document>))
  `(:html nil
          (:head nil
                 (:title nil "Common lisp spec"))
          (:body nil
                 ,(mapcar #'print-element (children element)))))

(defmethod print-element ((element <container-block-element>))
  (let* ((tag-name (name element))
         (tag (cond
                ((string= tag-name "section") :h2)
                ((string= tag-name "subsection") :h3)
                ((string= tag-name "subsubsection") :h4)
                ((string= tag-name "subsubsubsection") :h5)
                ((string= tag-name "subsubsubsubsection") :h6)
                )))
    `(:div (:class ,tag-name)
           (,tag nil ,(title element))
           ,(mapcar #'print-element (children element)))))


(defmethod print-element ((element <displaythree>))
  `(:div (:class "display-table--three")
         (:div (:class "display-table__title") ,(title element))
         (:ul (:class "display-table__list")
              ,(mapcar #'print-element (children element))
         )))

(defmethod print-element ((element <displaythree-func>))
  `(:li (:class "display-table__item") ,(name element)))

(defmethod print-element ((element <paragraph>))
  `(:p nil
       ,(mapcar #'print-element (children element))
       ))

(defmethod print-element ((element string)) element)

(defun get-link (type href text)
  (let ((class (format nil "link--~a" type)))
    `(:a (:class ,class :href ,href) ,text)))

(defmethod print-element ((element <term>))
  (get-link "term" (term element) (text element)))

(defmethod print-element ((element <new-term>))
  (get-link "new-term" (term element) (text element)))

(defmethod print-element ((element <seevar>))
  (get-link "var" (name element) (name element)))

(defmethod print-element ((element <seefuns>))
  (get-link "fun" (name element) (name element)))

(defmethod print-element ((element <funref>))
  (get-link "fun" (name element) (name element)))

(defmethod print-element ((element <varref>))
  (get-link "var" (name element) (name element)))

(defmethod print-element ((element <typeref>))
  (get-link "type" (name element) (name element)))

(defmethod print-element ((element <chapref>))
  (let ((name (name element)))
    (get-link "section" (name element)
              (or (gethash name (props (document element)))
                  "I DO NOT EXIST AND WILL THROW ERROR IN THE FUTURE"))))

(defmethod print-element ((element <metavar>))
  `(:i nil ,(name element)))

(defmethod print-element ((element <formula>))
  `(:formula nil
       ,(mapcar #'print-element (children element))
       ))

(defmethod print-element ((element <formula-sub>))
  `(:sub nil
             ,(mapcar #'print-element (children element))
             ))

(defmethod print-element ((element <formula-symbol>))
  `(:symbol nil ,(name element)))
