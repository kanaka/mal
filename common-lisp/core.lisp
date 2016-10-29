(defpackage :core
  (:use :common-lisp
        :types
        :printer)
  (:export :ns))

(in-package :core)

(defmacro wrap-boolean (form)
  `(if ,form
       types:mal-true
       types:mal-false))

(defun mal-add (value1 value2)
  (types:apply-unwrapped-values '+ value1 value2))

(defun mal-sub (value1 value2)
  (types:apply-unwrapped-values '- value1 value2))

(defun mal-mul (value1 value2)
  (types:apply-unwrapped-values '* value1 value2))

(defun mal-div (value1 value2)
  (types:make-mal-number (round (/ (types:mal-data-value value1)
                                   (types:mal-data-value value2)))))

(defun mal-prn (&rest strings)
  (format t
          "~{~a~^ ~}"
          (mapcar (lambda (string) (printer:pr-str string t))
                  strings))
  (terpri)
  (force-output *standard-output*)
  (types:make-mal-nil nil))

(defun mal-println (&rest strings)
  (format t
          "~{~a~^ ~}"
          (mapcar (lambda (string) (printer:pr-str string nil))
                  strings))
  (terpri)
  (force-output *standard-output*)
  (types:make-mal-nil nil))

(defun mal-pr-str (&rest strings)
  (types:make-mal-string (format nil
                                 "~{~a~^ ~}"
                                 (mapcar (lambda (string) (printer:pr-str string t))
                                         strings))))

(defun mal-str (&rest strings)
  (types:make-mal-string (format nil
                                 "~{~a~}"
                                 (mapcar (lambda (string) (printer:pr-str string nil))
                                         strings))))

(defun mal-list (&rest values)
  (make-mal-list values))

(defun mal-list? (value)
  (wrap-boolean (or (types:mal-nil-p value)
                    (types:mal-list-p value))))

(defun mal-empty? (value)
  (wrap-boolean (zerop (length (types:mal-data-value value)))))

(defun mal-length (value)
  (types:apply-unwrapped-values 'length value))

(defun mal-= (value1 value2)
  (wrap-boolean (types:mal-data-value= value1 value2)))

(defun mal-< (value1 value2)
  (types:apply-unwrapped-values-prefer-bool '<
                                            value1
                                            value2))

(defun mal-> (value1 value2)
  (types:apply-unwrapped-values-prefer-bool '>
                                            value1
                                            value2))

(defun mal-<= (value1 value2)
  (types:apply-unwrapped-values-prefer-bool '<=
                                            value1
                                            value2))

(defun mal->= (value1 value2)
  (types:apply-unwrapped-values-prefer-bool '>=
                                            value1
                                            value2))

(defvar ns
  (list
   (cons (types:make-mal-symbol "+") (types:make-mal-builtin-fn #'mal-add))
   (cons (types:make-mal-symbol "-") (types:make-mal-builtin-fn #'mal-sub))
   (cons (types:make-mal-symbol "*") (types:make-mal-builtin-fn #'mal-mul))
   (cons (types:make-mal-symbol "/") (types:make-mal-builtin-fn #'mal-div))
   (cons (types:make-mal-symbol "prn") (types:make-mal-builtin-fn #'mal-prn))
   (cons (types:make-mal-symbol "println") (types:make-mal-builtin-fn #'mal-println))
   (cons (types:make-mal-symbol "pr-str") (types:make-mal-builtin-fn #'mal-pr-str))
   (cons (types:make-mal-symbol "str") (types:make-mal-builtin-fn #'mal-str))
   (cons (types:make-mal-symbol "list") (types:make-mal-builtin-fn #'mal-list))
   (cons (types:make-mal-symbol "list?") (types:make-mal-builtin-fn #'mal-list?))
   (cons (types:make-mal-symbol "empty?") (types:make-mal-builtin-fn #'mal-empty?))
   (cons (types:make-mal-symbol "count") (types:make-mal-builtin-fn #'mal-length))
   (cons (types:make-mal-symbol "=") (types:make-mal-builtin-fn #'mal-=))
   (cons (types:make-mal-symbol "<") (types:make-mal-builtin-fn #'mal-<))
   (cons (types:make-mal-symbol ">") (types:make-mal-builtin-fn #'mal->))
   (cons (types:make-mal-symbol "<=") (types:make-mal-builtin-fn #'mal-<=))
   (cons (types:make-mal-symbol ">=") (types:make-mal-builtin-fn #'mal->=))))
