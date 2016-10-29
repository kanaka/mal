(defpackage :core
  (:use :common-lisp
        :utils
        :types
        :reader
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

(defun mal-read-string (value)
  (reader:read-str (types:mal-data-value value)))

(defun mal-slurp (filename)
  (types:apply-unwrapped-values 'read-file-string filename))

(defun mal-atom (value)
  (types:make-mal-atom value))

(defun mal-atom? (value)
  (wrap-boolean (types:mal-atom-p value)))

(defun mal-deref (atom)
  (types:mal-data-value atom))

(defun mal-reset! (atom value)
  (setf (types:mal-data-value atom) value))

(defun mal-swap! (atom fn &rest args)
  (setf (types:mal-data-value atom)
        (apply (types:mal-data-value fn)
               (append (list (types:mal-data-value atom))
                       args))))

(defun mal-cons (element list)
  (types:make-mal-list (cons element
                             (map 'list
                                  #'identity
                                  (types:mal-data-value list)))))

(defun mal-concat (&rest lists)
  (types:make-mal-list (apply #'concatenate
                              'list
                              (mapcar #'types:mal-data-value lists))))

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
   (cons (types:make-mal-symbol ">=") (types:make-mal-builtin-fn #'mal->=))
   (cons (types:make-mal-symbol "read-string") (types:make-mal-builtin-fn #'mal-read-string))
   (cons (types:make-mal-symbol "slurp") (types:make-mal-builtin-fn #'mal-slurp))
   (cons (types:make-mal-symbol "atom") (types:make-mal-builtin-fn #'mal-atom))
   (cons (types:make-mal-symbol "atom?") (types:make-mal-builtin-fn #'mal-atom?))
   (cons (types:make-mal-symbol "deref") (types:make-mal-builtin-fn #'mal-deref))
   (cons (types:make-mal-symbol "reset!") (types:make-mal-builtin-fn #'mal-reset!))
   (cons (types:make-mal-symbol "swap!") (types:make-mal-builtin-fn #'mal-swap!))
   (cons (types:make-mal-symbol "cons") (types:make-mal-builtin-fn #'mal-cons))
   (cons (types:make-mal-symbol "concat") (types:make-mal-builtin-fn #'mal-concat))))
