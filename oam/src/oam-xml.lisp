(oam:define-project-package #:ch.amann-wolowyk.oam-xml #:oxml)
(in-package #:ch.amann-wolowyk.oam-xml-system)


(defstruct (oxml::xml-element (:include s-xml::xml-element)
                              (:predicate oxml::xml-element-p)))

(defun check-xml-name (name)
  (assert (and name (symbolp name)) (name) "The name ~A is not a symbol." name)
  name)

(defun check-xml-attribute-key (key &optional pos)
  (assert (typep key 'symbol) (key) "The key ~A of the ~@[~:R ~]attribute is not a symbol." key pos)
  key)
(defun check-xml-attribute-value (key value &optional pos)
  (assert (typep value 'string) (value) "The value ~A of the ~@[~:R ~]attribute ~A is not a string." value pos key)
  value)
(defun check-xml-attributes (&rest attributes &key &allow-other-keys)
  (let (pos)
    (labels ((check (key val)
               (when pos (incf pos))
               (cons (check-xml-attribute-key key pos)
                     (check-xml-attribute-value key val pos))))
      (when (< 2 (length attributes))
        (setq pos 0))
      (oam:map-plist #'check attributes))))
(defun check-xml-value (value)
  (let (pos)
    (labels ((check (val)
               (when pos (incf pos))
               (when val
                 (assert (or (stringp val)
                             (oxml::xml-element-p val))
                         (val) "The ~@[~:R ~]value ~S of ~S is neither a string nor an instance of XML-ELEMENT." pos val value)
                 (list val))))
      (if (listp value)
        (setq pos 0)
        (setq value (list value)))
      (mapcan #'check value))))

(import '(xml-element-name check-xml-name check-xml-value check-xml-attributes)
                  (find-package "OXML"))
(defun oxml::make-xml-element (name &key attributes children)
  "Create and return a fresh instance of type XML-ELEMENT."
  (make-xml-element :name (check-xml-name name)
                    :attributes (apply #'check-xml-attributes attributes)
                    :children (check-xml-value children)))
(defun oxml::copy-xml-element (xml-element)
  "Return a deep copy of XML-ELEMENT."
  (unless (s-xml:xml-element-p xml-element)
    (error "The object ~A is not an XML-ELEMENT." xml-element))
  (make-xml-element :name (s-xml:xml-element-name xml-element)
                    :attributes (mapcar (lambda (attribute)
                                          (cons (car attribute)
                                                (copy-seq (cdr attribute))))
                                        (s-xml:xml-element-attributes xml-element))
                    :children (mapcar (lambda (child)
                                        (etypecase child
                                          (string (copy-seq child))
                                          (s-xml::xml-element (oxml::copy-xml-element child))))
                                      (s-xml:xml-element-children xml-element))))

(defun oxml::xml-element-attributes (xml-element)
  (copy-list (xml-element-attributes xml-element)))

(defun (setf oxml::xml-element-attributes) (value xml-element)
  (setf (xml-element-attributes xml-element) (apply #'check-xml-attributes value)))


(defun oxml::xml-element-attribute (xml-element key)
  (s-xml::xml-element-attribute xml-element key))

(defun (setf oxml::xml-element-attribute) (value xml-element key)
  (setf (s-xml::xml-element-attribute xml-element (check-xml-attribute-key key))
        (check-xml-attribute-value key value)))


(defun oxml::xml-element-value (xml-element)
  (let ((children (xml-element-children xml-element)))
    (if (< 1 (length children))
        (copy-list children)
        (first children))))

(defun (setf oxml::xml-element-value) (value xml-element)
  (setf (xml-element-children xml-element) (check-xml-value value)))




(defun oxml::xml-element-is-leaf-p (xml-element)
  (notany #'oxml::xml-element-p (xml-element-children xml-element)))

;;;;** Interface with methods

(defgeneric oxml::name (node))

(defmethod oxml::name ((node oxml::xml-element))
  (oxml::xml-element-name node))
(defmethod oxml::name ((nodes list))
  (mapcar #'oxml::name nodes))


(defgeneric (setf oxml::name) (value object))

(defmethod (setf oxml::name) (value (node oxml::xml-element))
  (setf (oxml::xml-element-name node) (check-xml-name value)))

(defmethod (setf oxml::name) (value (nodes list))
  (dolist (node nodes)
    (setf (oxml::name node) value)))


(defgeneric oxml::attribute (node key))

(defmethod oxml::attribute ((node oxml::xml-element) key)
  (oxml::xml-element-attribute node key))

(defgeneric (setf oxml::attribute) (value node key))

(defmethod (setf oxml::attribute) (value (node oxml::xml-element) key)
  (setf (oxml::xml-element-attribute node key) value))


(defgeneric oxml::value (object))

(defmethod oxml::value ((node oxml::xml-element))
  (oxml::xml-element-value  node))

(defmethod oxml::value ((nodes list))
  (mapcar #'oxml::value nodes))


(defgeneric (setf oxml::value) (value object))

(defmethod (setf oxml::value) (value (node oxml::xml-element))
  (setf (oxml::xml-element-value node) value))

(defmethod (setf oxml::value) (value (nodes list))
  (dolist (node nodes value)
    (setf (oxml::value node) value)))



(defmethod oxml::insert-values* ((xml-element oxml::xml-element) pos values)
  (setf (xml-element-children xml-element)
        (oam:insert pos (xml-element-children xml-element) (check-xml-value values)))
  xml-element)
(defmethod oxml::insert-values* ((nodes list) pos values)
  (dolist (node nodes)
    (oxml::insert-values* node pos values)))

(defun oxml::insert-values (node pos &rest values)
  (oxml::insert-values* node pos values))




(defgeneric oxml::parse-xml (input &key &allow-other-keys)
  (:documentation "Parse the xml from INPUT and create a fresh instance of a DOM of type XML-ELEMENT."))

(defmethod oxml::parse-xml ((input stream) &key &allow-other-keys)
  (oxml:copy-xml-element (s-xml:parse-xml-dom input :xml-struct)))
(defmethod oxml::parse-xml ((input string) &key &allow-other-keys)
  (with-input-from-string (stream input)
    (oxml::parse-xml stream)))
(defmethod oxml::parse-xml ((input pathname) &key &allow-other-keys)
  (with-open-file (stream input :direction :input)
    (oxml::parse-xml stream)))
(defmethod oxml::parse-xml ((input cons) &key (input-dom-type :sxml) &allow-other-keys)
  (oxml::parse-xml (s-xml:print-xml-string input :input-type input-dom-type)))
(defmethod oxml::parse-xml ((input oxml::xml-element) &key &allow-other-keys)
  (oxml::copy-xml-element input))

(defgeneric oxml::print-xml (xml-dom output &key  prettyp level &allow-other-keys)
  (:documentation "Print the XML-DOM object to OUTPUT."))

(defmethod oxml::print-xml ((xml-dom oxml::xml-element) (output stream) &key (prettyp t) (level 0)
                           &allow-other-keys)
  (s-xml:print-xml-dom xml-dom :xml-struct output prettyp level) nil)
(defmethod oxml::print-xml ((xml-dom cons) (output stream) &key (prettyp t) (level 0)
                           (xml-dom-type :sxml) &allow-other-keys)
  (assert (member xml-dom-type '(:sxml :lxml) :test #'eq))
  (s-xml:print-xml-dom xml-dom xml-dom-type output prettyp level) nil)
(defmethod oxml::print-xml (xml-dom (output null) &key (prettyp t) (level 0) (xml-dom-type :sxml) &allow-other-keys)
  (with-output-to-string (stream)
    (oxml::print-xml xml-dom stream :prettyp prettyp :level level :xml-dom-type xml-dom-type)))


(defclass oxml::path ()
  ())
(defgeneric oxml::find-xml-elements (xml-elements path &rest rest-path))

(defmethod oxml::find-xml-elements ((xml-elements null) path &rest rest-path)
  (declare (ignore xml-elements path rest-path)))

(defmethod oxml::find-xml-elements ((xml-elements cons) path &rest rest-path)
  (append (apply #'oxml::find-xml-elements (first xml-elements) path rest-path)
          (apply #'oxml::find-xml-elements (rest xml-elements) path rest-path)))

(defmethod oxml::find-xml-elements ((xml-elements oxml::xml-element) (path null) &rest rest-path)
  (declare (ignore path rest-path))
  (list xml-elements))

(defmethod oxml::find-xml-elements ((xml-elements oxml::xml-element) (path symbol) &rest rest-path)
  (oxml::find-xml-elements (oam::filter (lambda (xml-child)
                                          (when (eql path (xml-element-name xml-child))
                                            xml-child))
                                        (xml-element-children xml-elements))
                           (first rest-path) (rest rest-path)))



(oam:export-interface "OXML")