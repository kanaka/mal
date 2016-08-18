(load "types")

(defpackage :core
  (:use :common-lisp :types)
  (:export :ns))

(in-package :core)

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
                                                                                value2))))))
