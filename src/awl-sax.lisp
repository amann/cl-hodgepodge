;;;; -*- outline-regexp:";;;;[*]+ +" -*-
;;;;
#+debug
(declaim (optimize debug safety))
#-debug
(declaim (optimize speed))
;;;;
(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")
;;;;
;;;;* SAX XML Parser
;;;;** Parse Error
(define-condition awl::xml-parse-error ()
  ((stream :initarg :stream :reader awl::xml-parse-error-stream)
   (reason :initarg :reason :reader awl::xml-parse-error-reason))
  (:report (lambda (condition stream)
             (let* ((excerpt-length 6)
                    (xml-stream (awl::xml-parse-error-stream condition))
                    (pos (stream-file-position xml-stream)))
               (format stream "XML parse error at position ~D (~A|~A): ~A"
                       (progn
                         (stream-file-position xml-stream (max 0 (- pos excerpt-length)))
                         pos)
                       (let ((string (make-string excerpt-length))) (read-sequence string xml-stream) string)
                       (let ((string (make-string excerpt-length))) (read-sequence string xml-stream) string)
                       (progn (stream-file-position xml-stream pos)
                              (awl::xml-parse-error-reason condition)))))))

(defun xml-parse-error (stream control-string &rest args)
  (error 'awl::xml-parse-error :stream stream
         :reason (apply #'format nil control-string args)))

;;;;** SAX Events
;;;; Set of SAX events implemented as conditions.
(define-condition event () ())

(define-condition sax-event (event) ())

(define-condition sax-on-xml-element (sax-event)
  ((stack :initarg :stack :reader xml-element-stack)))
(defun sax-on-xml-element-name (sax-event)
  (first (xml-element-stack object)))
(defun sax-on-xml-element-path (sax-event)
    (format nil "~{/~A~}" (reverse (xml-element-stack object)))  )
(defgeneric awl::xml-element-name (object)
  (:method ((object sax-on-xml-element))
    (sax-on-xml-element-name object)))
(defgeneric awl::xml-element-path (object)
  (:method ((object sax-on-xml-element))
    (sax-on-xml-element-path object)))
;;;;*** SAX public events
(define-condition awl::sax-on-xml-element-start (sax-on-xml-element) ()
  (:report (lambda (condition stream)
             (format stream "Start element ~A." (sax-on-xml-element-path condition)))))
(define-condition awl::sax-on-xml-element-end (sax-on-xml-element) ()
  (:report (lambda (condition stream)
             (format stream "End element ~A." (sax-on-xml-element-path condition)))))
(define-condition awl::sax-on-attribute (sax-on-xml-element)
  ((key :initarg :key :reader xml-attribute-key)
   (value :initarg :value :reader xml-attribute-value))
  (:report (lambda (condition stream)
             (format stream "Attribute ~A@~A='~A'."
                     (sax-on-xml-element-path condition)
                     (xml-attribute-key condition)
                     (xml-attribute-value condition)))))
(define-condition awl::sax-on-text (sax-on-xml-element)
  ((text :initarg :text :reader xml-element-text))
  (:report (lambda (condition stream &aux (display-length 15))
             (let* ((text (xml-element-text condition))
                    (text-length (length text)))
               (format stream "Text of element ~A: \"~A~:[~;...~]\""
                       (sax-on-xml-element-path condition)
                       (subseq text 0 (min text-length display-length))
                       (< display-length text-length))))))

(define-condition awl::sax-on-processing-instruction (sax-event)
  ((target :initarg :target :reader xml-pi-target)
   (data :initarg :data :reader xml-pi-data))
  (:report (lambda (condition stream &aux (display-length 15))
             (let* ((data (xml-pi-data condition))
                    (data-length (length data)))
               (format stream "Processing instruction ~A: \"~A~:[~;...~]\""
                       (xml-pi-target condition)
                       (subseq data 0 (min data-length display-length))
                       (< display-length data-length))))))

;;;;** Character Patterns
(defvar *xml-context*)
(defun xml-invalid-context (stream char)
  (xml-parse-error stream "Character ~A invalid in context ~A."
                   char *xml-context*))
;;;; A range is of the form
;;;; range ::= char-des | (char-des . char-des)
;;;;
(defconstant +∞ '+∞)
(defconstant -∞ '-∞)

(defun ≤ (a b)
    (cond ((or (eq a -∞) (eq b +∞)) t)
          ((or (eq a +∞) (eq b -∞)) nil)
          ((consp a) (not (≤ (if (consp b) (car b) b) (car a))))
          ((consp b) (not (≤ (car b) a)))
          (t (<= a b))))
(defun %intersect-intervals (a b)
  (flet ((min* (a b)
           (if (≤ a b) a b)))
    (flet ((lo (i) (or (car i) -∞))
           (hi (i) (or (cdr i) +∞))
           (intersect (lb ha hb)
             (when (≤ lb ha)
               (cons lb (min* ha hb)))))
      (declare (inline lo hi intersect))
      (if (≤ (lo a) (lo b))
          (intersect (lo b) (hi a) (hi b))
          (intersect (lo a) (hi b) (hi a))))))

(defclass character-range ()
  (list))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-char-code (char-designator)
    (etypecase char-designator
      (character (char-code char-designator))
      ((or integer pair) char-designator)
      ((cons * atom) (pair (ensure-char-code (car char-designator))
                           (ensure-char-code (cdr char-designator))))))
  (defun expand-char-pattern (pattern)
    (flet ((ensure-char-code (char-designator)
             (handler-case
                 `,(ensure-char-code char-designator)
               (type-error  () `(expand-char-pattern ,char-designator)))))
      (awl:map-tree #'ensure-char-code pattern)))

  (defmacro make-char-pattern (&body patterns)
    `(flatten (eval-tree ,(expand-char-pattern patterns)))))

(defmacro do-all-characters (var &body body)
  `(dotimes (i #xF0000)
     (let ((,var (code-char i)))
       ,@body)))

;;;;*** XML Syntax Character Ranges
(defparameter +xml-whitespace+ '(#\Space #\Tab #\Return #\Newline))
(defparameter +xml-name-start-chars+ (make-char-pattern
                                       #\: (#\A . #\Z) #\_ (#\a . #\z)
                                       (#xC0 . #xD6) (#xD8 . #xF6)
                                       (#xF8 . #x2FF)  (#x370 . #x37D)
                                       (#x37F . #x1FFF)  (#x200C . #x200D)
                                       (#x2070 . #x218F)  (#x2C00 . #x2FEF)
                                       (#x3001 . #xD7FF)  (#xF900 . #xFDCF)
                                       (#xFDF0 . #xFFFD)  (#x10000 . #xEFFFF)))
(defparameter +xml-name-chars+ (make-char-pattern
                                 +xml-name-start-chars+
                                 #\- #\. (#\0 . #\9) #xB7 (#x0300 . #x036F) (#x203F . #x2040)))
(defvar *xml-syntax-valid-chars*)
(defun xml-valid-char-p (char &optional (valid-chars *xml-syntax-valid-chars*))
  (some (let ((char-code (etypecase char
                           (character (char-code char))
                           (integer char))))
          (lambda (pattern)
            (typecase pattern
              (cons (<= (car pattern) char-code (cdr pattern)))
              (t (= char-code pattern)))))
        valid-chars))
(defun xml-valid-name (string)
  (every 'xml-valid-char-p string (cons +xml-name-start-chars+ (make-circle +xml-name-chars+))))
(declaim (inline spacep))
(defun spacep (char &optional (readtable *readtable*))
  (flet ((test (string &aux (*readtable* readtable))
           (with-input-from-string (*standard-input* string)
             (not (ignore-errors (peek-char t))))))
    (declare (inline test))
    (etypecase char
      (character (test (string char)))
      (string (test char))
      (integer (test (string (code-char char)))))))
(declaim (inline non-terminating-p)
         (ftype (function (character &optional readtable)) non-terminating-p))
(defun non-terminating-p (char &optional (readtable *readtable*))
  (declare (type character char))
  (not (or (spacep char readtable)
           (and (get-macro-character char)
                (not (nth-value 1 (get-macro-character char)))))))
;;;;** Names
;;;;*** Namespaces
;;;; Namespaces are packages which are registered in a package-registry.
(defvar awl::*xml-namespaces*)
(declaim (type package-registry awl::*xml-namespaces*))
(defun awl::xml-namespace-package (namespace)
  (registry-find-package namespace awl::*xml-namespaces*))
(defun awl::xml-namespace (name)
  (registry-package-name (symbol-package name)))

(defun token-reader (stream char)
  (with-output-to-string (*standard-output*)
    (write-char char)
    (loop :while (non-terminating-p (peek-char nil stream t nil t))
       :do (write-char (read-char stream t nil t)))))

(defun string-invert (string)
  (cond
    ((every (lambda (char) (char= char (char-upcase char))) string)
     (string-downcase string))
    ((every (lambda (char) (char= char (char-downcase char))) string)
     (string-upcase string))
    (t string)))

(defun xml-name-reader (stream char)
  (declare (type character char)
           (type stream stream))
  (let* ((ns-sep (when (boundp 'awl::*xml-namespaces*) #\:))
         (token (funcall (case (readtable-case *readtable*)
                           (:upcase #'string-upcase)
                           (:downcase #'string-downcase)
                           (:invert #'string-invert)
                           (t #'identity)) (token-reader stream char)))
         (pos (unless (eql char ns-sep) (position ns-sep token))))
    (declare (type (or fixnum null) pos)
             (type (simple-array character) token))
    (intern (subseq token (1+ (the fixnum (or pos -1))))
            (if pos (awl::xml-namespace-package (subseq token 0 pos)) *package*))))

;;;;** XML Entities
(defparameter +xml-predefined-entities+ '((#\& . amp)
                                          (#\< . lt)
                                          (#\> . gt)
                                          (#\' . apos)
                                          (#\" . quot)))
(defvar *xml-entities* nil)
(defun xml-find-entity (id)
  (or (car (rassoc id (append +xml-predefined-entities+
                              *xml-entities*)))
      (ignore-errors (code-char id))
      (format nil "&~A;" id)))
(defun xml-write-char-reference (char)
  (funcall (formatter "&~(~A~);") nil
           (or (cdr (assoc char +xml-predefined-entities+))
               (format nil "#x~16R" (char-code char)))))
(defun make-xml-entity-ref-readtable (&aux (*readtable* (copy-readtable nil)))
  (let ((hex-reader (get-dispatch-macro-character #\# #\x)))
    (do-all-characters char
      (if (xml-valid-char-p char +xml-name-start-chars+)
          (set-macro-character char #'xml-name-reader t)
          (set-macro-character char #'xml-invalid-context t)))
    (set-dispatch-macro-character #\# #\x hex-reader))
  (set-macro-character #\; (lambda (stream char) (declare (ignore stream)) char))
  (set-dispatch-macro-character #\# #\; (lambda (s c n) (unread-char c s) n))
  *readtable*)
(defparameter +xml-entity-ref-readtable+ (make-xml-entity-ref-readtable))
(defun xml-entity-ref-reader (stream char)
  (declare (ignore char))
  (let* ((*readtable* +xml-entity-ref-readtable+)
         (*xml-context* "entity ref")
         (token (read stream t nil t)))
    (unless (eql #\; (read stream t nil t))
      (error 'xml-parse-error :stream stream
             :reason "Invalid Entity."))
    (xml-find-entity token)))

;;;;** XML String Reader

(defvar *xml-element-stack*)
(defvar *xml-current-element-name*)
(defvar *xml-attribute-key*)

(defun expand-fun (function-designator args)
  (etypecase function-designator
    (function `(funcall ,function-designator ,@args))
    ((or symbol awl:lambda-expression) `(,function-designator ,@args))))

(defun make-token-reader (termination-char-p escape-char-p escape-reader)
  (compile nil
           `(lambda (stream char)
              (flet ((termination-char-p (char)
                       ,(expand-fun termination-char-p '(char)))
                     (escape-char-p (char)
                       ,(expand-fun escape-char-p '(char)))
                     (escape-reader (stream char)
                       ,(expand-fun escape-reader '(stream char))))
                (with-output-to-string (*standard-output*)
                  (loop :for char := (read-char stream t nil t)
                     :until (termination-char-p char)
                     :do (if (escape-char-p char)
                             (write (escape-reader stream char))
                             (write-char char))))))))

(defun xml-string-reader (stream char)
  (with-output-to-string (*standard-output*)
    (let ((termination-char char)
          (escape-char #\&))
     (loop :for char := (read-char stream t nil t)
        :until (char= char termination-char)
        :do (if (char= char escape-char)
                (write (xml-entity-ref-reader stream char))
                (write-char char))))))

(defun sax-text-reader (stream char)
  (declare (ignore char))
  (when *xml-element-stack*
    (let ((text (xml-string-reader stream #\<)))
      (unread-char #\< stream)
      (when (plusp (length text))
        (signal 'sax-on-text :stack *xml-element-stack* :text text)))))

;;;;** Toplevel

(defun make-xml-toplevel-readtable (&aux (*readtable* (copy-readtable nil)))
  (do-all-characters char
    (set-macro-character char #'xml-invalid-context))
  (dolist (char +xml-whitespace+)
    (set-syntax-from-char char #\Space))
  (set-macro-character #\< #'xml-tag-reader)
  *readtable*)

(defparameter +xml-toplevel-readtable+ (make-xml-toplevel-readtable)
  "Syntax of the toplevel where only tags and whitespaces are allowed.")

(defun awl::sax-parse-xml (stream)
  (let (*xml-element-stack* (*xml-context* "toplevel")
        (*readtable* +xml-toplevel-readtable+))
    (handler-case
        (loop (read stream))
      (end-of-file ()))))

;;;;** Tag Reader

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun xml-tag-reader (stream char)
   (declare (ignore char))
   (let ((char (read-char stream t nil t)))
     (case char
       (#\? (xml-pi-reader stream char))
       (#\! (xml-!-reader stream char))
       (#\/ (xml-/-reader stream char))
       (t (xml-element-reader stream char))))))

;;;;** XML-Element Reader

(defun make-xml-element-readtable (&aux (*readtable* (copy-readtable nil)))
  (do-all-characters char
    (if (xml-valid-char-p char +xml-name-start-chars+)
        (set-macro-character char #'xml-name-reader t)
        (set-macro-character char #'xml-invalid-context
                             (xml-valid-char-p char +xml-name-chars+))))
  (dolist (char +xml-whitespace+)
    (set-syntax-from-char char #\Space))
  (set-macro-character #\> #'sax-text-reader)
  (setf (readtable-case *readtable*) :preserve)
  *readtable*)
(defparameter +xml-element-readtable+ (make-xml-element-readtable))
(defun make-xml-element-contents-readtable (&aux (*readtable* (copy-readtable +xml-element-readtable+)))
  (set-macro-character #\< #'xml-tag-reader)
  *readtable*)
(defparameter +xml-element-contents-readtable+ (make-xml-element-contents-readtable))
;;;;*** XML Attributes
(defun make-xml-attribute-readtable (&aux (*readtable* (copy-readtable +xml-element-readtable+)))
  (set-macro-character #\" #'xml-string-reader)
  (set-macro-character #\' #'xml-string-reader)
  (set-macro-character #\= (lambda (stream char)
                             (declare (ignore char))
                             (if *xml-attribute-key*
                                 (read stream t nil t)
                                 (error 'xml-parse-error :stream stream
                                        :reason "No key given."))))
  (set-macro-character #\/ #'xml-/-reader)
  (set-macro-character #\> (lambda (stream char)
                             (throw :end-of-tag
                               (unread-char char stream))))
  *readtable*)
(defparameter +xml-attribute-readtable+ (make-xml-attribute-readtable))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun xml-/-reader (stream char)
    (declare (ignore char))
    (let ((current-element-name (catch :end-of-tag (read stream t nil t))))
      (if (or (null current-element-name)
              (eq *xml-current-element-name* current-element-name))
          (throw :end-of-element (signal 'sax-on-xml-element-end
                                         :stack *xml-element-stack*))
          (xml-parse-error stream "Illegal nesting of element ~S in ~{/~S~}."
                           current-element-name (reverse *xml-element-stack*))))))

(defun xml-element-reader (stream char)
  (unread-char char stream)
  (catch :end-of-element
    (let* ((*readtable* +xml-element-readtable+)
           (*xml-context* "xml-element")
           (*xml-current-element-name* (read stream t nil t))
           (*xml-element-stack* (cons *xml-current-element-name* *xml-element-stack*)))
      (signal 'sax-on-xml-element-start :stack *xml-element-stack*)
      (catch :end-of-tag
        (let ((*readtable* +xml-attribute-readtable+)
              (*xml-context* "attribute"))
          (loop (let* ((*xml-attribute-key* (read stream t nil t)))
                  (signal 'sax-on-attribute
                          :stack *xml-element-stack*
                          :key *xml-attribute-key*
                          :value (read stream t nil t))))))
      (let ((*readtable* +xml-element-contents-readtable+))
        (loop (read stream t nil t)))))
  (read-char stream t nil t))

;;;;** Process Instructions
(defun xml-pi-reader (stream char)
  (let ((*readtable* +xml-element-readtable+))
    (signal 'sax-on-processing-instruction
            :target (read stream t nil t)
            :data (progn (peek-char t stream t nil t) (xml-string-reader stream char)))
    (let ((char (read-char stream t nil t)))
      (unless (char= char #\>)
        (xml-parse-error stream "Invalid character at the end of PI: ~A." char)))))
;;;;** ! Reader
(defun xml-!-reader (stream char)
  (declare (ignore stream char))
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-symbols (sym '#:awl)
    (export sym '#:awl)))





