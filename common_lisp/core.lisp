(require "types")
(require "reader")
(require "printer")

(defpackage :core
  (:use :common-lisp :types :reader :printer)
  (:export :ns))

(in-package :core)

(defun get-file-contents (filename)
  (with-open-file (stream filename)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defvar ns
  (list
   (cons (types:make-mal-symbol '+)
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values '+ value1 value2))))

   (cons (types:make-mal-symbol '-)
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values '- value1 value2))))

   (cons (types:make-mal-symbol '*)
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values '* value1 value2))))

   (cons (types:make-mal-symbol '/)
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values '/ value1 value2))))

   (cons (types:make-mal-symbol '|prn|)
         (types:make-mal-builtin-fn (lambda (&rest strings)
                                      (write-line (format nil
                                                          "~{~a~^ ~}"
                                                          (mapcar (lambda (string) (printer:pr-str string t))
                                                                  strings)))
                                      (types:make-mal-nil nil))))

   (cons (types:make-mal-symbol '|println|)
         (types:make-mal-builtin-fn (lambda (&rest strings)
                                      (write-line (format nil
                                                          "~{~a~^ ~}"
                                                          (mapcar (lambda (string) (printer:pr-str string nil))
                                                                  strings)))
                                      (types:make-mal-nil nil))))

   (cons (types:make-mal-symbol '|pr-str|)
         (types:make-mal-builtin-fn (lambda (&rest strings)
                                      (types:make-mal-string (format nil
                                                                     "~{~a~^ ~}"
                                                                     (mapcar (lambda (string) (printer:pr-str string t))
                                                                             strings))))))

   (cons (types:make-mal-symbol '|str|)
         (types:make-mal-builtin-fn (lambda (&rest strings)
                                      (types:make-mal-string (format nil
                                                                     "~{~a~}"
                                                                     (mapcar (lambda (string) (printer:pr-str string nil))
                                                                             strings))))))

   (cons (types:make-mal-symbol '|list|)
         (types:make-mal-builtin-fn (lambda (&rest values)
                                      (make-mal-list values))))

   (cons (types:make-mal-symbol '|list?|)
         (types:make-mal-builtin-fn (lambda (value)
                                      (types:make-mal-boolean (or (types:mal-nil-p value)
                                                                  (types:mal-list-p value))))))

   (cons (types:make-mal-symbol '|empty?|)
         (types:make-mal-builtin-fn (lambda (value)
                                      (types:make-mal-boolean (zerop (length (mal-value value)))))))

   (cons (types:make-mal-symbol '|count|)
         (types:make-mal-builtin-fn (lambda (value)
                                      (types:apply-unwrapped-values 'length value))))

   (cons (types:make-mal-symbol '=)
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:make-mal-boolean (types:mal-value= value1 value2)))))

   (cons (types:make-mal-symbol '<)
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values-prefer-bool '<
                                                                                value1
                                                                                value2))))

   (cons (types:make-mal-symbol '>)
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values-prefer-bool '>
                                                                                value1
                                                                                value2))))

   (cons (types:make-mal-symbol '<=)
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values-prefer-bool '<=
                                                                                value1
                                                                                value2))))

   (cons (types:make-mal-symbol '>=)
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values-prefer-bool '>=
                                                                                value1
                                                                                value2))))

   (cons (types:make-mal-symbol '|read-string|)
         (types:make-mal-builtin-fn (lambda (value)
                                      (reader:read-str (types:mal-value value)))))

   (cons (types:make-mal-symbol '|slurp|)
         (types:make-mal-builtin-fn (lambda (filename)
                                      (types:apply-unwrapped-values 'get-file-contents filename))))

   (cons (types:make-mal-symbol '|atom|)
         (types:make-mal-builtin-fn (lambda (value)
                                      (types:make-mal-atom value))))

   (cons (types:make-mal-symbol '|atom?|)
         (types:make-mal-builtin-fn (lambda (value)
                                      (types:make-mal-boolean (types:mal-atom-p value)))))

   (cons (types:make-mal-symbol '|deref|)
         (types:make-mal-builtin-fn (lambda (atom)
                                      (types:mal-value atom))))

   (cons (types:make-mal-symbol '|reset!|)
         (types:make-mal-builtin-fn (lambda (atom value)
                                      (setf (types:mal-value atom) value))))

   (cons (types:make-mal-symbol '|swap!|)
         (types:make-mal-builtin-fn (lambda (atom fn &rest args)
                                      (setf (types:mal-value atom)
                                            (apply (mal-value fn)
                                                   (append (list (types:mal-value atom))
                                                           args))))))

   (cons (types:make-mal-symbol '|cons|)
         (types:make-mal-builtin-fn (lambda (element list)
                                      (types:make-mal-list (cons element
                                                                 (map 'list
                                                                      #'identity
                                                                      (mal-value list)))))))

   (cons (types:make-mal-symbol '|concat|)
         (types:make-mal-builtin-fn (lambda (&rest lists)
                                      (types:make-mal-list (apply #'concatenate
                                                                  'list
                                                                  (mapcar #'types:mal-value lists))))))


   (cons (types:make-mal-symbol '|nth|)
         (types:make-mal-builtin-fn (lambda (sequence index)
                                      (or (nth (mal-value index)
                                               (map 'list #'identity (mal-value sequence)))
                                          (error "Index out of range")))))

   (cons (types:make-mal-symbol '|first|)
         (types:make-mal-builtin-fn (lambda (sequence)
                                      (or (first (map 'list #'identity (mal-value sequence)))
                                          (types:make-mal-nil nil)))))

   (cons (types:make-mal-symbol '|rest|)
         (types:make-mal-builtin-fn (lambda (sequence)
                                      (types:make-mal-list (rest (map 'list
                                                                      #'identity
                                                                      (mal-value sequence)))))))))
