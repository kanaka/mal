(defpackage :env
  (:use :common-lisp :types)
  (:export :undefined-symbol
           :mal-env
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

(define-condition arity-mismatch (types:mal-runtime-exception)
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

(defun create-mal-env (&key (parent nil) (binds nil) (exprs nil))
  (let ((varidiac-position (position (types:make-mal-symbol "&")
                                     binds
                                     :test #'mal-value=)))
    (when varidiac-position
      (setf (subseq binds varidiac-position (length binds))
            (list (nth (1+ varidiac-position) binds)))
      (setf binds (subseq binds 0 (1+ varidiac-position)))

      (let* ((no-of-args (length exprs))
             ;; There are enough arguments for variadic operator
             ;; to consume
             (rest-args (cond ((>= no-of-args (1+ varidiac-position))
                               (make-mal-list (subseq exprs
                                                      varidiac-position
                                                      (length exprs))))
                              ;; There are enough parameters to satisfy the
                              ;; normal arguments, set rest-args to a nil value
                              ((= no-of-args varidiac-position)
                               types:mal-nil))))
        (handler-case
            (setf exprs (concatenate 'list
                                     (subseq exprs 0 varidiac-position)
                                     (list rest-args)))
          (simple-type-error (condition)
            (error 'arity-mismatch
                   :required (length binds)
                   :provided (length exprs))))))

    (when (not (= (length binds) (length exprs)))
      (error 'arity-mismatch
             :required (length binds)
             :provided (length exprs)))

    (let ((arg-params (map 'list #'cons binds exprs))
          (bindings (make-hash-table :test 'equal)))
      (dolist (arg-param arg-params)
        (setf (gethash (types:mal-data-value (car arg-param)) bindings)
              (cdr arg-param)))
      (make-mal-env :bindings bindings :parent parent))))
