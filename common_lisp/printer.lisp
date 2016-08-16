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
                       (map 'list #'pr-str (types:mal-value sequence)))
               end-delimiter))

(defun pr-mal-hash-map (hash-map)
  (let ((hash-map-value (types:mal-value hash-map)))
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
    (let ((repr (switch-mal-type ast
                  ('types:number (format nil "~d" (types:mal-value ast)))
                  ('types:boolean (if (types:mal-value ast) "true" "false"))
                  ('types:nil "nil")
                  ('types:string (format nil "~s" (types:mal-value ast)))
                  ('types:symbol (format nil "~a" (types:mal-value ast)))
                  ('types:keyword (format nil ":~a" (types:mal-value ast)))
                  ('types:list (pr-mal-sequence "(" ast ")"))
                  ('types:vector (pr-mal-sequence "[" ast "]"))
                  ('types:hash-map (pr-mal-hash-map ast))))
          (meta (pr-str (types:mal-meta ast))))
      (if meta
          (format nil "(with-meta ~a ~a)" repr meta)
          repr))))
