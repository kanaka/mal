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

(defun pr-mal-hash-map (hash-map)
  (let ((hash-map-value (types::mal-value hash-map)))
    (concatenate 'string
                 "{"
                 (format nil
                         "~{~A~^ ~}"
                         (mapcar (lambda (key-value)
                                   (format nil
                                           "~a ~a"
                                           (pr-str (car key-value))
                                           (pr-str (cdr key-value))))
                                 (loop
                                    for key being the hash-keys of hash-map-value
                                    collect (cons key (gethash key hash-map-value)))))
                 "}")))

(defun pr-str (ast)
  (when ast
    (switch-mal-type ast
      ('number (format nil "~d" (types::mal-value ast)))
      ('boolean (if (types::mal-value ast) "true" "false"))
      ('nil "nil")
      ('string (format nil "~s" (types::mal-value ast)))
      ('symbol (format nil "~a" (types::mal-value ast)))
      ('keyword (format nil ":~a" (types::mal-value ast)))
      ('list (pr-mal-sequence "(" ast ")"))
      ('vector (pr-mal-sequence "[" ast "]"))
      ('hash-map (pr-mal-hash-map ast)))))
