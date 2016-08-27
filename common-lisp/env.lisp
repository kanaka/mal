(defpackage :env
  (:use :common-lisp)
  (:export :undefined-symbol))

(in-package :env)

(define-condition undefined-symbol (error)
  ((symbol :initarg :symbol :reader env-symbol))
  (:report (lambda (condition stream)
             (format stream
                     "'~a' not found"
                     (env-symbol condition)))))
