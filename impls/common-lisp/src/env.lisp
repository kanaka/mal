(defpackage :env
  (:use :common-lisp :types)
  (:shadow :symbol)
  (:export :undefined-symbol
           :create-mal-env
           :get-env
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

(defun get-env (env symbol)
  (or (gethash symbol (mal-env-bindings env))
      (let ((outer (mal-env-parent env)))
        (if outer
          (get-env outer symbol)
          nil))))

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
