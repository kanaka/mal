(require "types")

(defpackage :env
  (:use :common-lisp :types)
  (:export :undefined-symbol
           :mal-environment
           :get-env
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
  (let ((varidiac-position (position (types:make-mal-symbol '&)
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
                               (make-mal-nil nil)))))
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

    (let ((arg-params (map 'list #'cons binds exprs)))
      (dolist (arg-param arg-params)
        (set-env env
                 (car arg-param)
                 (cdr arg-param))))))
