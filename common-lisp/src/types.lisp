(defpackage :types
  (:use :common-lisp
        :genhash)
  (:import-from :utils
                :listify)
  (:export :mal-data-value=
           ;; Accessors
           :mal-data-value
           :mal-data-type
           :mal-data-meta
           :mal-data-attrs
           ;; Mal values
           :number
           :make-mal-number
           :mal-number-p

           :boolean
           :mal-boolean-p

           :nil
           :mal-nil-p

           :string
           :make-mal-string
           :mal-string-p

           :symbol
           :make-mal-symbol
           :mal-symbol-p

           :keyword
           :make-mal-keyword
           :mal-keyword-p

           :list
           :make-mal-list
           :mal-list-p

           :vector
           :make-mal-vector
           :mal-vector-p

           :hash-map
           :make-mal-hash-map
           :mal-hash-map-p

           :atom
           :make-mal-atom
           :mal-atom-p

           :builtin-fn
           :make-mal-builtin-fn
           :mal-builtin-fn-p

           :fn
           :make-mal-fn
           :mal-fn-p

           :any
           :switch-mal-type

           ;; Singleton values
           :mal-nil
           :mal-true
           :mal-false

           ;; Hashing mal values
           :make-mal-value-hash-table
           ;; Error types
           :mal-exception
           ;; Exceptions raised by the runtime
           :mal-runtime-exception
           ;; Exception raised by user code
           :mal-user-exception
           ;; Error
           :mal-error

           ;; Utilities
           :wrap-value
           :unwrap-value
           :apply-unwrapped-values
           :apply-unwrapped-values-prefer-bool))

(in-package :types)

(define-condition mal-error (error)
  nil)

(define-condition mal-exception (error)
  nil)

(define-condition mal-runtime-exception (mal-exception)
  nil)

(define-condition mal-user-exception (mal-exception)
  ((data :accessor mal-exception-data :initarg :data)))

(defstruct mal-data
  (value nil)
  (type nil :read-only t)
  meta
  attrs)

;; Create a constructor and predicate for given type
(defmacro define-mal-type (type)
  (let ((constructor (intern (format nil "MAKE-MAL-~a" (symbol-name type))))
        (predicate (intern (format nil "MAL-~a-P" (symbol-name type)))))
    `(progn (defun ,constructor (value &key meta attrs)
              (make-mal-data :type ',type
                             :value value
                             :meta meta
                             :attrs attrs))

            (defun ,predicate (value)
              (when (typep value 'mal-data)
                (eq (mal-data-type value) ',type))))))

(define-mal-type number)
(define-mal-type symbol)
(define-mal-type keyword)
(define-mal-type string)
(define-mal-type boolean)
(define-mal-type nil)

(define-mal-type list)
(define-mal-type vector)
(define-mal-type hash-map)

(define-mal-type atom)

(define-mal-type fn)
(define-mal-type builtin-fn)

(defvar mal-nil (make-mal-nil nil))
(defvar mal-true (make-mal-boolean t))
(defvar mal-false (make-mal-boolean nil))

;; Generic type
(defvar any)

(defmacro switch-mal-type (ast &body forms)
  `(let ((type (mal-data-type ,ast)))
     (cond
       ,@(mapcar (lambda (form)
                   (list (if (or (equal (car form) t)
                                 (equal (car form) 'any))
                             t
                             (list 'equal (list 'quote (car form)) 'type))
                         (cadr form)))
                 forms))))

(defun mal-sequence= (value1 value2)
  (let ((sequence1 (utils:listify (mal-data-value value1)))
        (sequence2 (utils:listify (mal-data-value value2))))
    (when (= (length sequence1) (length sequence2))
      (every #'identity
             (loop
                for x in sequence1
                for y in sequence2
                collect (mal-data-value= x y))))))

(defun mal-hash-map= (value1 value2)
  (let ((map1 (mal-data-value value1))
        (map2 (mal-data-value value2))
        (identical t))
    (when (= (genhash:generic-hash-table-count map1)
             (genhash:generic-hash-table-count map2))
      (genhash:hashmap (lambda (key value)
                         (declare (ignorable value))
                         (setf identical
                               (and identical (mal-data-value= (genhash:hashref key map1)
                                                               (genhash:hashref key map2)))))
                       map1)
      identical)))

(defun mal-data-value= (value1 value2)
  (when (and (typep value1 'mal-data)
             (typep value2 'mal-data))

    (if (equal (mal-data-type value1) (mal-data-type value2))
        (switch-mal-type value1
          (list (mal-sequence= value1 value2))
          (vector (mal-sequence= value1 value2))
          (hash-map (mal-hash-map= value1 value2))
          (any (equal (mal-data-value value1) (mal-data-value value2))))
        (when (or (and (mal-list-p value1) (mal-vector-p value2))
                  (and (mal-list-p value2) (mal-vector-p value1)))
          (mal-sequence= value1 value2)))))

(defun mal-sxhash (value)
  (sxhash (mal-data-value value)))

(defun make-mal-value-hash-table ()
  (unless (gethash 'mal-data-value-hash genhash::*hash-test-designator-map*)
    ;; ECL and ABCL's implementations of sxhash do not work well with compound
    ;; types, use a custom hash function which hashes the underlying value
    ;; instead
    (let ((hash-function #+(or ecl abcl) #'mal-sxhash
                         #-(or ecl abcl) #'sxhash))
      (genhash:register-test-designator 'mal-data-value-hash
                                        hash-function
                                        #'mal-data-value=)))
  (genhash:make-generic-hash-table :test 'mal-data-value-hash))

(defun wrap-value (value &key booleanp listp)
  (typecase value
    (number (make-mal-number value))
    ;; This needs to be before symbol since nil is a symbol
    (null (funcall (cond
                     (booleanp #'make-mal-boolean)
                     (listp #'make-mal-list)
                     (t #'make-mal-nil))
                   value))
    ;; This needs to before symbol since t, nil are symbols
    (boolean (make-mal-boolean value))
    (keyword (make-mal-keyword value))
    (symbol (make-mal-symbol (symbol-name value)))
    (string (make-mal-string value))
    (list (make-mal-list (map 'list #'wrap-value value)))
    (vector (make-mal-vector (map 'vector #'wrap-value value)))
    (hash-table (make-mal-hash-map (let ((new-hash-table (make-mal-value-hash-table)))
                                     (genhash:hashmap (lambda (key value)
                                                        (setf (genhash:hashref (wrap-value key) new-hash-table)
                                                              (wrap-value value)))
                                                      value)
                                     new-hash-table)))))

(defun unwrap-value (value)
  (switch-mal-type value
    (list (mapcar #'unwrap-value (mal-data-value value)))
    (vector (map 'vector #'unwrap-value (mal-data-value value)))
    (hash-map (let ((hash-table (make-hash-table))
                    (hash-map-value (mal-data-value value)))
                (genhash:hashmap (lambda (key value)
                                   (setf (genhash:hashref (mal-data-value key) hash-table)
                                         (mal-data-value value)))
                                 hash-map-value)
                hash-table))
    (any (mal-data-value value))))

(defun apply-unwrapped-values (op &rest values)
  (wrap-value (apply op (mapcar #'unwrap-value values))))

(defun apply-unwrapped-values-prefer-bool (op &rest values)
  (wrap-value (apply op (mapcar #'unwrap-value values)) :booleanp t))
