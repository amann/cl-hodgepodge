;;;;* Context
;;;;
;;;; A Context is a first class lexical environment on the function and variable name spaces.
;;;; It is implemented as a set of variable locations which are a cons of a two closures, one
;;;; to get the value and one to set the value of the variable location.


(oam:define-project-package "CH.AMANN-WOLOWYK.CONTEXT" "CON")
(in-package "CH.AMANN-WOLOWYK.CONTEXT-SYSTEM")

(defun oam::bindablep (o)
  (and (symbolp o) (not (constantp o))))
(deftype oam::bindable ()
  '(satisfies oam::bindablep))

(deftype oam::pair (&optional (first t) (second first))
  `(oam::ntuple ,first ,second))
(deftype oam::ntuple (&rest elt-types)
  (if (zerop (length elt-types))
      'null
      `(cons ,(car elt-types) (oam::ntuple ,@(cdr elt-types)))))

(oam:defmacro! oam::specialp (sym)
  `(let ((,g!f (lambda () ,sym)))
     (progv '(,sym) '(nil)
       (makunbound ',sym)
       (handler-case (progn (funcall ,g!f) nil)
         (error () t)))))
(defmacro oam::lexicalp (sym)
  `(and (ignore-errors ,sym t)
        (not (oam::specialp ,sym))))

(defclass lexical-context ()
  ((variables :reader variables)
   (functions :reader functions)
   (macros :reader macros)))

(defmethod initialize-instance :after ((self lexical-context) &key variables functions macros
                                       &allow-other-keys)
  (let ((update-slot (lambda (slot-name list)
                       (let ((hash (make-hash-table :size (length list))))
                         (mapc (lambda (item) (setf (gethash (car item) hash) (cdr item))) list)
                         (setf (slot-value self slot-name) hash)))))
    (mapc update-slot '(variables functions macros) (list variables functions macros))))

(declaim (inline getvar var-value getfun getmacfun))
(defun getvar (context symb)
  (gethash symb (variables context)))

(defun var-value (var)
  (funcall (car var)))
(defsetf var-value (var) (value)
  `(funcall (cdr ,var) ,value))

(defun getfun (context symb)
  (gethash symb (functions context)))
(defun getmacfun (context symb)
  (gethash symb (macros context)))


(flet ((normate-syms (syms)
         (mapcar (lambda (sym)
                   (etypecase sym
                     (oam::bindable (list sym sym nil))
                     ((oam::ntuple oam::bindable) (list (first sym) (first sym) nil))
                     ((oam::pair oam::bindable) (append sym nil))
                     ((oam::pair oam::bindable (member :var :fun)) (list (first sym) (first sym) (second sym)))
                     ((oam::ntuple oam::bindable oam::bindable (member :var :fun)) sym)))
                 syms))
       (filter-syms (types syms)
         (let ((result (remove-duplicates (oam:filter (lambda (sym) (when (member (third sym) types) (butlast sym))) syms)
                                          :test #'equal)))
           (assert (= (length result) (length (remove-duplicates result :key #'car))) ()
                   "Ambigous (duplicate) name assignement for ~@[function ~]variables." (member :fun types))
           result))
       (make-symb-vars (fn syms context)
         (oam:filter (lambda (sym) (let ((var (funcall fn context (second sym))))
                                     (when var (cons (first sym) `(quote ,var)))))
                     syms)))  
  (oam:defmacro! con::capture-environment (syms)
    (let* ((syms (normate-syms syms))
           (var-syms (filter-syms '(nil :var) syms))
           (fun-syms (filter-syms '(:fun) syms))
           (mac-syms (filter-syms '(nil :fun :mac) syms)))
      `(macrolet ((,g!get-macro-fun (symb &environment env) `(macro-function ,symb ,env)))
         (let* ((symb.vars (delete-if #'null (list ,@(mapcar (lambda (sym)
                                                               `(when (oam::lexicalp ,(second sym))
                                                                  (cons ',(first sym) (cons (lambda () ,(second sym))
                                                                                            (lambda (,g!val) (setf ,(second sym) ,g!val))))))
                                                             var-syms))))
                (symb.funs (delete-if #'null (list ,@(mapcar (lambda (sym)
                                                               `(let ((fun (function ,(second sym))))
                                                                  (when fun
                                                                    (cons ',(first sym) fun))))
                                                             fun-syms))))
                (symb.macfuns (delete-if #'null (list ,@(mapcar (lambda (sym)
                                                                  `(let ((macfun (,g!get-macro-fun ',(second sym))))
                                                                     (when macfun
                                                                       (cons ',(first sym) (cons macfun ',(second sym))))))
                                                                mac-syms)))))
           (make-instance 'lexical-context
                          :variables symb.vars
                          :functions symb.funs
                          :macros symb.macfuns)))))
  (labels
      ((expand-var-env (symb.vars body)
         `(symbol-macrolet (,@(mapcar (lambda (symb.var)
                                        `(,(car symb.var) (var-value ,(cdr symb.var))))
                                      symb.vars))
            ,@body))
       (expand-full-env (symb.vars symb.funs symb.macfuns body)
         `(flet (,@(mapcar (lambda (symb.fun)
                             `(,(car symb.fun) (&rest args)
                                (apply ,(caddr symb.fun) args)))
                           symb.funs))
            (macrolet (,@(mapcar (lambda (symb.macfun)
                                   (let ((macfun (caddr symb.macfun)))
                                     `(,(car symb.macfun) (&rest args &environment env)
                                        (funcall ,(car macfun) (cons ',(cdr macfun) args) env))))
                                 symb.macfuns))
              ,(expand-var-env symb.vars body)))))
    (defun %make-bindings (syms body context &aux (var-syms (filter-syms '(nil :var) (normate-syms syms))))
      (let* ((g!context (gensym "CONTEXT"))
             (gensym-vars (mapcar (lambda (sym)
                                    (let ((cell-name (second sym)))
                                      `(,(gensym (string cell-name)) (or (getvar ,g!context ',cell-name)
                                                                         (error 'cell-error :name ',cell-name)))))
                                  var-syms))
             (symb.gensyms (mapcar (lambda (sym gensym-var)
                                     (cons (first sym) (first gensym-var)))
                                   var-syms gensym-vars)))
        `(let ((,g!context ,context))
           (let (,@gensym-vars)
             ,(expand-var-env symb.gensyms body)))))
    (defun %make-full-bindings (syms body context &aux
                                (syms (normate-syms syms))
                                (var-syms (filter-syms '(nil :var) syms))
                                (fun-syms (filter-syms '(nil :fun) syms)))
      (let ((symb-vars (make-symb-vars #'getvar var-syms context))
            (symb-funs (make-symb-vars #'getfun fun-syms context))
            (symb-macfuns (make-symb-vars #'getmacfun fun-syms context)))
        (expand-full-env symb-vars symb-funs symb-macfuns body)))))

(defmacro con::with-context ((&rest syms) context &body body)
  (%make-bindings syms body context))

(defvar con::*context* nil)
(defmacro con::with-full-current-context (&body body &environment env)
  `(eval (when (typep con::*context* 'lexical-context ,env)
           (%make-full-bindings ',(oam:filter (lambda (o) (when (oam::bindablep o) o)) (oam:flatten body))
                                ',body con::*context*))))

(defmacro con::with-full-context (context &body body)
  `(let ((con::*context* ,context))
     (con::with-full-current-context ,@body)))



(defvar *named-contexts* (make-hash-table))
;;;;TODO: How can we distinguish the compile-time environment from the run-time environment?
(defun con::find-context (context-name &optional (errorp t) env)
  (declare (ignore env))
  (or (gethash context-name *named-contexts*)
      (when errorp
        (error "No context with name ~S found." context-name))))
(defsetf con::find-context (context-name &optional errorp env) (context)
  `(typecase ,context
       (lexical-context (setf (gethash ,context-name *named-contexts*) ,context))
       (t ,errorp ,env (remhash ,context-name *named-contexts*) ,context)))

(defmacro con::define-context (name (&rest syms))
  `(setf (find-context ,name) (con::capture-environment ,syms)))


(defmacro con::with-full-named-context ((&rest syms) context-name &body body &environment env)
  (%make-full-bindings (or syms (oam:filter (lambda (o) (when (oam::bindablep o) o)) (oam:flatten body)))
                       body (con::find-context context-name t env)))



(oam:export-interface "CON")