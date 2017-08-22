(defpackage :env
  (:use :common-lisp :types)
  (:shadow :symbol)
  (:export :undefined-symbol
           :create-mal-env
           :get-env
           :find-env
           :set-env
           :mal-env-bindings))

(in-package :env)

(define-condition undefined-symbol (mal-runtime-exception)
  ((symbol :initarg :symbol :reader symbol))
  (:report (lambda (condition stream)
             (format stream
                     "'~a' not found"
                     (symbol condition)))))

(define-condition arity-mismatch (mal-runtime-exception)
  ((required :initarg :required :reader required)
   (provided :initarg :provided :reader provided))
  (:report (lambda (condition stream)
             (format stream
                     "Unexpected number of arguments provided, expected ~a, got ~a"
                     (required condition)
                     (provided condition)))))

(defstruct mal-env
  (bindings (make-hash-table :test 'equal) :read-only t)
  (parent nil :read-only t))

(defun find-env (env symbol)
  (when env
    (or (gethash (mal-data-value symbol)
                 (mal-env-bindings env))
        (find-env (mal-env-parent env) symbol))))

(defun get-env (env symbol)
  (or (find-env env symbol)
      (error 'undefined-symbol
             :symbol (format nil "~a" (mal-data-value symbol)))))

(defun set-env (env symbol value)
  (setf (gethash (mal-data-value symbol) (mal-env-bindings env)) value))

(defun create-mal-env (&key parent binds exprs)
  (let ((env (make-mal-env :parent parent))
        (params-length (length binds))
        (arg-length (length exprs)))

    (flet ((arity-mismatch ()
             (error 'arity-mismatch
                    :required params-length
                    :provided arg-length)))
      (loop
         for key = (pop binds)
         while key
         do (if (string/= (mal-data-value key) "&")
                (set-env env key (or (pop exprs)
                                     (arity-mismatch)))
                (progn (set-env env
                                (or (pop binds) (arity-mismatch))
                                (make-mal-list exprs))
                       (setq binds nil))))
      env)))
