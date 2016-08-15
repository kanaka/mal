(require "types")

(defpackage :printer
  (:use :common-lisp :types)
  (:export :pr-str))

(in-package :printer)

(defun pr-mal-sequence (start-delimiter sequence end-delimiter)
  (concatenate 'string
               start-delimiter
               (format nil
                       "~{~A~^ ~}"
                       (mapcar #'pr-str (types::mal-value sequence)))
               end-delimiter))

(defun pr-str (ast)
  (when ast
    (case (types::mal-type ast)
      (types::number (format nil "~d" (types::mal-value ast)))
      (types::boolean (if (types::mal-value ast) "true" "false"))
      (types::nil "nil")
      (types::string (format nil "~s" (types::mal-value ast)))
      (types::symbol (format nil "~a" (types::mal-value ast)))
      (types::list (pr-mal-sequence "(" ast ")"))
      (types::vector (pr-mal-sequence "[" ast "]")))))
