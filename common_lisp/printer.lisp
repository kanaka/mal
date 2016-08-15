(require "types")

(defpackage :printer
  (:use :common-lisp :types)
  (:export :pr-str))

(in-package :printer)

(defun pr-str (ast)
  (when ast
    (case (types::mal-type ast)
      ('number (format nil "~d" (types::mal-value ast)))
      ('boolean (if (types::mal-value ast) "true" "false"))
      ('nil "nil")
      ('string (format nil "~s" (types::mal-value ast)))
      ('symbol (format nil "~a" (types::mal-value ast)))
      ('list (concatenate 'string
                          "("
                          (format nil
                                  "~{~A~^ ~}"
                                  (mapcar #'pr-str (types::mal-value ast)))
                          ")")))))
