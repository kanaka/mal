(require "types")

(defpackage :env
  (:use :common-lisp :types)
  (:export :lookup-env
           :undefined-symbol))

(in-package :env)

(define-condition undefined-symbol (error)
  ((symbol :initarg :symbol :reader symbol))
  (:report (lambda (condition stream)
             (format stream
                     "Symbol ~a is undefined"
                     (symbol condition)))))
