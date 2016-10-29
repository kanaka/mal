(defpackage :env
  (:use :common-lisp :types)
  (:shadow :symbol)
  (:export :undefined-symbol
           :create-mal-env
           :get-env
           :find-env
           :set-env))

(in-package :env)

(define-condition undefined-symbol (types:mal-runtime-exception)
  ((symbol :initarg :symbol :reader symbol))
  (:report (lambda (condition stream)
             (format stream
                     "'~a' not found"
                     (symbol condition)))))

(defstruct mal-env
  (bindings (make-hash-table :test 'equal) :read-only t)
  (parent nil :read-only t))

(defun find-env (env symbol)
  (let ((value (gethash (types:mal-data-value symbol)
                        (mal-env-bindings env)))
        (parent (mal-env-parent env)))
    (cond
      (value value)
      (parent (find-env parent symbol))
      (t nil))))

(defun get-env (env symbol)
  (let ((value (find-env env symbol)))
    (if value
        value
        (error 'undefined-symbol
               :symbol (format nil "~a" (types:mal-data-value symbol))))))

(defun set-env (env symbol value)
  (setf (gethash (types:mal-data-value symbol)
                 (mal-env-bindings env))
        value))

(defun create-mal-env (&key (parent nil))
  (make-mal-env :parent parent))
