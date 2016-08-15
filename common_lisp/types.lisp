(defpackage :types
  (:use :common-lisp))

(in-package :types)

(defclass mal-type ()
  ((value :accessor mal-value :initarg :value)
   (type :accessor mal-type :initarg :type)))

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

(define-mal-type list)
(define-mal-type vector)
(define-mal-type number)
(define-mal-type symbol)
(define-mal-type string)
(define-mal-type boolean)
(define-mal-type nil)
