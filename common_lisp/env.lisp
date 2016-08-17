(require "types")

(defpackage :env
  (:use :common-lisp :types)
  (:export :undefined-symbol
           :mal-environment
           :get-env
           :set-env))

(in-package :env)

(define-condition undefined-symbol (error)
  ((symbol :initarg :symbol :reader symbol))
  (:report (lambda (condition stream)
             (format stream
                     "Symbol ~a is undefined"
                     (symbol condition)))))

(defclass mal-environment ()
  ((bindings :initarg :bindings
             :accessor mal-env-bindings
             :initform (make-hash-table :test 'types:mal-value=))
   (parent :initarg :parent
           :accessor mal-env-parent
           :initform nil)))

(defgeneric find-env (mal-environment symbol)
  (:documentation "Find value of a symbol in given environment, return nil if not binding is found"))

(defgeneric get-env (mal-environment symbol)
  (:documentation "Get value of a symbol in given environment, raises undefined-symbol error if lookup fails"))

(defgeneric set-env (mal-environment symbol value)
  (:documentation "Set the value for a symbol in given environment"))

(defmethod find-env ((env mal-environment) symbol)
  (let ((value (gethash symbol (mal-env-bindings env)))
        (parent (mal-env-parent env)))
    (cond
      (value value)
      (parent (find-env parent symbol))
      (t nil))))

(defmethod get-env ((env mal-environment) symbol)
  (let ((value (find-env env symbol)))
    (if value
        value
        (error 'undefined-symbol
               :symbol (format nil "~a" (types:mal-value symbol))))))

(defmethod set-env ((env mal-environment) symbol value)
  (setf (gethash symbol (mal-env-bindings env)) value))

(defmethod initialize-instance :after ((env mal-environment)
                                       &key (bindings nil)
                                         (parent nil)
                                         (binds nil)
                                         (exprs nil))
  (let ((arg-params (loop
                       for x in binds
                       for y in exprs
                       collect (cons x y))))
    (dolist (arg-param arg-params)
      (set-env env
               (car arg-param)
               (cdr arg-param)))))
