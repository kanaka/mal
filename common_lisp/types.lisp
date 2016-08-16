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
           :any
           ;; Helpers
           :apply-unwrapped-values
           :switch-mal-type))

(in-package :types)

(defclass mal-type ()
  ((value :accessor mal-value :initarg :value)
   (type :accessor mal-type :initarg :type)))

(defmethod print-object ((obj mal-type) out)
  (with-slots (value type) obj
    (format out "#<mal ~a: ~a>" type value)))

(defun mal-value= (value1 value2)
  (and (equal (mal-type value1) (mal-type value2))
       (equal (mal-value value1) (mal-value value2))))

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
                                                         (symbol-name type))))))
    `(progn (defclass ,name (mal-type)
              ((type :accessor mal-type
                     :initarg :type
                     :initform ',type)))

            (defun ,constructor (value)
              (make-instance ',name
                             :value value))

            (export ',name)
            (export ',constructor))))

(define-mal-type number)
(define-mal-type symbol)
(define-mal-type keyword)
(define-mal-type string)
(define-mal-type boolean)
(define-mal-type list)
(define-mal-type vector)
(define-mal-type hash-map)
(define-mal-type nil)

(defmacro switch-mal-type (ast &body forms)
  `(let ((type (types::mal-type ,ast)))
     (cond
       ,@(mapcar (lambda (form)
                   (list (list 'equal (car form) 'type)
                         (cadr form)))
                 forms))))
