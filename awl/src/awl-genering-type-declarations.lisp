;;;; -*- outline-regexp:";;;;[*]+ +" -*-

(in-package "CH.AMANN-WOLOWYK.AWL-SYSTEM")


;;;;============================================================================
;;;;*


;;;;** Controlling declarations
(defvar awl::*typespec-check-hook* nil
  "List of functions taking as single argument an expression and returning either NIL or a valid typespec.")

(defvar awl::*default-typespec* 'number)

(defun awl::get-effective-typespec (typespec)
  "Take an \"extended\" typespec, i.e. a valid typespec or an expression which can be transformed to a valid typespec by one of the functions in awl::*typespec-check-hook*, and return a valid typespec. If the expression resulting by applying awl::*typespec-check-hook* is not a valid typespec, an error is signalled."
  (if (awl::type-spec-p typespec)
      typespec
      (let ((typespec (when (boundp 'awl::*typespec-check-hook*)
                        (dolist (fn awl::*typespec-check-hook*)
                          (let ((typespec (funcall fn typespec)))
                            (when typespec (return typespec)))))))
        (if (awl::type-spec-p typespec)
            typespec
            (let ((typespec (or (when (boundp 'awl::*default-typespec*) awl::*default-typespec*)
                                t)))
              (check-type typespec awl::type-spec "a valid typespec")
              typespec)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun awl::get-var-expr-typespec (binding&typespec)
    "Parse BINDING&TYPESPEC, which must be of the form var | (var &optional expr typespec), and return the three values var, expr and effective-typespec, where effective-typespec has been determined by the function get-effective-typespec."
    (multiple-value-bind (var expr typespec)
        (etypecase binding&typespec
          (awl::variable (values binding&typespec binding&typespec))
          ((cons awl::variable list) (values (first binding&typespec)
                                            (or (second binding&typespec)
                                                (first binding&typespec))
                                            (third binding&typespec))))
      (let ((typespec (awl::get-effective-typespec typespec)))
        (values var expr typespec)))))


(defmacro awl::with-vars (bindings&typespecs &body body)
  "Locally bind variables with a declared type. BINDINGS&TYPESPECS is a list of bindings. Each binding and type declaration is of the form var | (var &optional expr typespec) where var is a variable name, expr is an expression and typespec a \"generalized\" typespec."
  (let ((bindings (mapcar (lambda (b&t)
                            (multiple-value-bind (var val typespec)
                                (etypecase b&t
                                  (awl::variable (values b&t))
                                  ((cons awl::variable list) (values-list b&t)))
                              (let ((g!var (gensym (string var)))
                                    (typespec (awl::get-effective-typespec (or typespec t))))
                                (list `(,g!var ,(if (eq typespec t) val `(coerce ,val ',typespec)))
                                      `(type ,typespec ,g!var)
                                      `(,g!var () ,g!var)
                                      `((setf ,g!var) (v) (setq ,g!var (coerce v ',typespec)))
                                      g!var
                                      `(ftype (function () ,typespec) ,g!var)
                                      `(,var (,g!var))))))
                          bindings&typespecs)))
    `(let ,(mapcar #'first bindings)
       (declare ,@(mapcar #'second bindings))
       (flet (,@(mapcar #'third bindings)
              ,@(mapcar #'fourth bindings))
         (declare (inline ,@(mapcar #'fifth bindings))
                  ,@(mapcar #'sixth bindings))
         (symbol-macrolet ,(mapcar #'seventh bindings)
           ,@body)))))


(defun awl::clean-declarations (code-expr)
  (awl::map-tree (lambda (expr)
                   (when (consp expr)
                     (case (car expr)
                       (declare (clean-declare expr))
                       (the (clean-the expr)))))
                 code-expr))

(defun clean-declare (declarations)
  (labels ((recursive-fun (decls)
             (let ((rest-decl (member 'type decls :key #'car)))
               (when rest-decl
                 (let ((type-decl (first rest-decl)))
                   (setf (second type-decl) (awl::get-effective-typespec (second type-decl))))
                 (recursive-fun rest-decl)))))
    (recursive-fun (rest declarations)))
  declarations)

(defun clean-the (the-invocation)
  (setf (second the-invocation) (awl::get-effective-typespec (second the-invocation)))
  the-invocation)



;;;;** Number Types


(defvar *default-float-format* *read-default-float-format*
  "Default float format used for computations.")

(defvar *addition-groups* '((complex long-float) (complex double-float) (complex single-float) (complex short-float)
                             (complex integer) (complex rational) complex
                             long-float double-float single-float short-float float
                             integer rational real number t)
  "List of types which represent addition groups of numbers.")


(defun dispatch-type (type &optional (domain :addition-group))
  (ecase domain
    (:addition-group (addition-group type))
    (:quotient-field (quotient-field type))
    (:cauchy-complete (cauchy-complete type))))

(defvar awl::*enumeration-format-string*
  "~{~#[ none~; ~S~; ~S and ~S~:;~@{~#[~; and~] ~S~^,~}~]~}")

(defun resulting-type* (domain types)
  (dispatch-type (dolist (elt-type *addition-groups*
                          (error (concatenate 'string "The type~P" awl::*enumeration-format-string* "~2:* ~[are~;is~:;are~] incompatible.")
                                 (length types) types))
                   (when (some (lambda (type) (subtypep type elt-type)) types)
                     (return elt-type)))
                 domain))
(defun resulting-type (domain &rest types)
  (resulting-type* domain types))


(defun addition-group (type)
  (dolist (suptype *addition-groups*)
    (when (subtypep type suptype)
      (return suptype))))
(defun quotient-field (type)
  (let ((type (addition-group type)))
    (cond ((subtypep type 'rational) 'rational)
          ((subtypep type '(complex rational)) '(complex rational))
          (t type))))
(defun cauchy-complete (type)
  (let ((type (quotient-field type)))
    (cond ((subtypep type 'rational) 'long-float)
          ((subtypep type '(complex rational)) '(complex long-float))
          (t type))))




#+ (or)
(defun %make-typed-function (result-names result-types input-names input-types body context captured-symbols)
  (let* ((result-types (mapcar (lambda (rt) (awl::map-tree (lambda (expr)
                                                             (when (and (consp expr)
                                                                        (eq 'type-of (first expr)))
                                                               (let ((pos (position (second expr) input-names)))
                                                                 (when pos (setf (first expr) 'quote
                                                                                 (second expr) (nth pos input-types))))))
                                                           rt)) result-types))
         (names (append result-names input-names))) 
    (funcall (compile nil
                      `(lambda ()
                         (declare ,*speed-optimizations*)
                         (con::with-context (,@captured-symbols) ,context
                                            (list (lambda ,names
                                                    (declare ,*speed-optimizations*) 
                                                    ,@(awl::clean-declarations body)
                                                    (values ,@result-names))
                                                  (list ,@result-types ))))))))

#+ (or)
(defmacro define-typed-function (name (&rest input) (&rest result) &body options&body)
  "(define-typed-function foo (a b) ((c (make-foo (bar a) (foo-bar b)) (resulting-type :quotient-field (type-of a) (type-of b))))
     (let ((d (coerce 0 (type-of c))))
       (declare (type (type-of c) d))
       (setf d (the (type-of c) (/ a b))) etc..)) 
Be careful: the symbol cl:declare and cl:the should not be used else than in the standard context of declarations, since the code in the body is parsed in a primitive way."
  (let* ((input-spec (mapcar (lambda (in)
                               (etypecase in
                                 (awl::variable (list in #'type-of))
                                 ((awl::pair awl::variable function)
                                  (let* ((arglist (awl::function-arg-list (second in)))
                                         (mandatory-args (awl::get-mandatory-args arglist))
                                         (optional-args (awl::get-optional-args arglist)))
                                    (assert (or (= 1 (length mandatory-args))
                                                (and (zerop (length mandatory-args))
                                                     (< 0 (length optional-args)))) ()
                                            "The type-defining function must have exactly one mandatory or at least one optional argument ~
                                             but the actual arglist is ~S" arglist)
                                    in))))
                             input))
         (result-specs (mapcar (awl::dlambda ((name constructor &optional (type t)))
                                            (list name type
                                                  `(lambda ,input (declare (ignorable ,@input)) ,constructor)))
                               result))
         (function-name (awl::mkgnsym name)))
    (multiple-value-bind (options body)
        (loop :while (member (first (first options&body)) '(:check-input :documentation :capture-environment))
           :do (push (pop options&body) options)
           :finally (return options options&body))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (let* ((captured-symbols (rest (assoc :capture-environment options)))
                (context (con:capture-environment captured-symbols))
                (dispatch-table (make-hash-table :test #'equal)))
           (defun ,function-name (input)
             (let* ((input-types (mapcar #'funcall ',(mapcar #'second input-spec)))
                    (dispatched-types (mapcar #'dispatch-type input-types))) 
               (values-list (or (gethash dispatched-types dispatch-table)
                                (setf (gethash dispatched-types dispatch-table) (%make-operation ',(mapcar #'first result-specs)
                                                                                                 ',(mapcar #'second result-specs)
                                                                                                 ',(mapcar #'first input-spec)
                                                                                                 dispatched-types
                                                                                                 ',body
                                                                                                 ',context ',captured-symbols)))))))
         (defun ,name ()
           ,(second (assoc :documentation options))
           (values ',function-name
                   ',input
                   (list ,@(mapcar #'third result-specs))
                   (let* ((check (rest (assoc :check-input options)))
                          (expr (first check))
                          (datum (rest check)))
                     (lambda ,input
                       (assert ,@(or expr '(t)) () ,@datum)))))))))

;;;; !!!! result specs should be lambda expressions !!!!
#+ (or)
(defmacro with-typed-function (name (&rest result-input) &body body)
  (multiple-value-bind (operator input-old-names result-constructors check-input)
      (funcall (symbol-function name))
    (let* ((length-input-old-names (length input-old-names)))
      (let ((difference (- (length result-input) length-input-old-names (length result-constructors))))
        (assert (zerop difference) ()
                "In ~S: ~D Argument~:P ~:[not enough~:;too much~]. ~D result and ~D input variables needed."
                result-input (abs difference) (plusp difference) (length result-constructors) length-input-old-names))
      (let* ((result-names (butlast result-input length-input-old-names))
             (input-bindings (mapcar #'awl::var-binding-pair (last result-input length-input-old-names)))
             (input-names (mapcar #'first input-bindings))
             (input-g!names (mapcar #'awl::mkgnsym input-names))
             (input-bindings (mapcar (lambda (g!name binding)
                                       (list g!name (second binding)))
                                     input-g!names input-bindings))
             (input-names-g!names (mapcar #'list input-names input-g!names))
             (input-oldnames-g!names (mapcar #'list input-old-names input-g!names))
             (results-name.spec (mapcar #'cons result-names result-constructors)))
        (awl::with-gensyms (g!input g!input-types g!op g!result-types
                                   g!nbr-rows g!nbr-cols g!elt-type)
          `(let ,input-bindings
             (funcall ,check-input ,@input-g!names)
             (multiple-value-bind (,g!op ,g!result-types)
                 (,operator ,input-g!names)
               (flet ((,name (,@result-names ,@input-names)
                        (funcall ,g!op ,@result-names ,@input-names)))
                 (let ,input-names-g!names
                   (declare (ignorable ,@input-names))
                   ,@body)))))))))


