(defpackage :printer
  (:use :common-lisp
        :types)
  (:import-from :genhash
                :hashmap)
  (:import-from :cl-ppcre
                :regex-replace)
  (:import-from :utils
                :replace-all
                :listify)
  (:export :pr-str))

(in-package :printer)

(defun pr-mal-sequence (start-delimiter sequence end-delimiter &optional (print-readably t))
  (format nil
          "~a~{~a~^ ~}~a"
          start-delimiter
          (mapcar (lambda (value)
                    (pr-str value print-readably))
                  (utils:listify (types:mal-data-value sequence)))
          end-delimiter))

(defun pr-mal-hash-map (hash-map &optional (print-readably t) &aux repr)
  (genhash:hashmap (lambda (key value)
                     (push (pr-str value print-readably) repr)
                     (push (pr-str key   print-readably) repr))
                   (types:mal-data-value hash-map))
  (format nil "{~{~a ~a~^ ~}}" repr))

(defun pr-string (ast &optional (print-readably t))
    (if print-readably
      (utils:replace-all (prin1-to-string (types:mal-data-value ast))
                         "
"
                         "\\n")
      (types:mal-data-value ast)))

(defun pr-str (ast &optional (print-readably t))
  (when ast
    (switch-mal-type ast
      (types:number (format nil "~d" (types:mal-data-value ast)))
      (types:boolean (if (types:mal-data-value ast) "true" "false"))
      (types:nil "nil")
      (types:string (pr-string ast print-readably))
      (types:symbol (format nil "~a" (types:mal-data-value ast)))
      (types:keyword (format nil "~a" (types:mal-data-value ast)))
      (types:list (pr-mal-sequence "(" ast ")" print-readably))
      (types:vector (pr-mal-sequence "[" ast "]" print-readably))
      (types:hash-map (pr-mal-hash-map ast print-readably))
      (types:atom (format nil "(atom ~a)" (pr-str (types:mal-data-value ast))))
      (types:builtin-fn "#<func>")
      (types:builtin-fn "#<builtin-func>"))))
