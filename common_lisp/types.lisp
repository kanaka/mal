(defpackage :types
  (:use :common-lisp)
  (:export :mal-value=
           ;; Accessors
           :mal-value
           :mal-type
           :mal-meta
           ;; Mal values
           :number
           :boolean
           :nil
           :string
           :symbol
           :keyword
           :list
           :vector
           :hash-map
           :fn
           :builtin-fn
           :any
           ;; Helpers
           :wrap-value
           :apply-unwrapped-values
           :switch-mal-type))

(in-package :types)

(defclass mal-type ()
  ((value :accessor mal-value :initarg :value)
   (meta :accessor mal-meta :initarg :meta :initform nil)
   (type :accessor mal-type :initarg :type)))

(defmethod print-object ((obj mal-type) out)
  (with-slots (value type meta) obj
    (format out "#<mal ~a: ~a (~a)>" type value meta)))

(defun mal-value= (value1 value2)
  (and (equal (mal-type value1) (mal-type value2))
       (if (mal-symbol-p value1)
           (string= (symbol-name (mal-value value1))
                    (symbol-name (mal-value value2)))
           (equal (mal-value value1) (mal-value value2)))))

(defun hash-mal-value (value)
  (sxhash (mal-value value)))

#+sbcl (sb-ext:define-hash-table-test mal-value= hash-mal-value)
#+clisp (ext:define-hash-table-test mal-value= mal-value= hash-mal-value)

(defmacro define-mal-type (type)
  ;; Create a class for given type and a convenience constructor and also export
  ;; them
  (let ((name (intern (string-upcase (concatenate 'string
                                                  "mal-"
                                                  (symbol-name type)))))
        (constructor (intern (string-upcase (concatenate 'string
                                                         "make-mal-"
                                                         (symbol-name type)))))
        (predicate (intern (string-upcase (concatenate 'string
                                                         "mal-"
                                                         (symbol-name type)
                                                         "-p")))))
    `(progn (defclass ,name (mal-type)
              ((type :accessor mal-type
                     :initarg :type
                     :initform ',type)))

            (defun ,constructor (value &optional meta)
              (make-instance ',name
                             :value value
                             :meta meta))
            (defun ,predicate (value)
              (when (typep value 'mal-type)
                (equal (mal-type value) ',type)))

            (export ',name)
            (export ',constructor)
            (export ',predicate))))

(define-mal-type number)
(define-mal-type symbol)
(define-mal-type keyword)
(define-mal-type string)
;; TODO true, false and nil should ideally be singleton
(define-mal-type boolean)
(define-mal-type nil)

(define-mal-type list)
(define-mal-type vector)
(define-mal-type hash-map)

(define-mal-type fn)
(define-mal-type builtin-fn)

;; Generic type
(defvar any)

(defmacro switch-mal-type (ast &body forms)
  `(let ((type (types:mal-type ,ast)))
     (cond
       ,@(mapcar (lambda (form)
                   (list (if (or (equal (car form) t)
                                 (equal (car form) 'any))
                             t
                             (list 'equal (list 'quote (car form)) 'type))
                         (cadr form)))
                 forms))))

(defun wrap-value (value)
  (funcall (typecase value
             (number #'make-mal-number)
             (symbol #'make-mal-number)
             (keyword #'make-mal-keyword)
             (string #'make-mal-string)
             (boolean #'make-mal-boolean)
             (list #'make-mal-list)
             (vector #'make-mal-vector)
             (hash-map #'make-mal-hash-map)
             (null #'make-mal-nil))
           value))

(defun apply-unwrapped-values (op &rest values)
  (wrap-value (apply op (mapcar #'mal-value values))))
