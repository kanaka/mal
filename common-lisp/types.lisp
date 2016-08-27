(defpackage :types
  (:use :common-lisp
        :genhash)
  (:export ;; Accessors
           :mal-data-value
           :mal-data-type
           :mal-data-meta
           :mal-data-attrs
           ;; Mal values
           :number
           :make-mal-number
           :mal-number-p

           :boolean
           :make-mal-boolean
           :mal-boolean-p

           :nil
           :make-mal-nil
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

           :any

           :switch-mal-type

           ;; Hashing mal values
           :make-mal-value-hash-table

           ;; Utilities
           :apply-unwrapped-values))

(in-package :types)

(defstruct mal-data
  (value nil :read-only t)
  (type nil :read-only t)
  meta
  attrs)

;; Create a constructor and predicate for given type
(defmacro define-mal-type (type)
  (let ((constructor (intern (string-upcase (concatenate 'string
                                                         "make-mal-"
                                                         (symbol-name type)))))
        (predicate (intern (string-upcase (concatenate 'string
                                                       "mal-"
                                                       (symbol-name type)
                                                       "-p")))))
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
;; TODO true, false and nil should ideally be singleton
(define-mal-type boolean)
(define-mal-type nil)

(define-mal-type list)
(define-mal-type vector)
(define-mal-type hash-map)

(define-mal-type atom)

(define-mal-type builtin-fn)

;; Generic type
(defvar any)

(defun mal-data-value= (value1 value2)
  (equal (mal-data-value value1)
         (mal-data-value value2)))

(defun make-mal-value-hash-table ()
  (unless (gethash 'mal-data-value-hash genhash::*hash-test-designator-map*)
    (genhash:register-test-designator 'mal-data-value-hash
                                      #'sxhash
                                      #'mal-data-value=))
  (genhash:make-generic-hash-table :test 'mal-data-value-hash))

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
                                     (loop
                                      for key being the hash-keys of value
                                      do (setf (gethash (wrap-value key) new-hash-table)
                                               (wrap-value (gethash key value))))
                                     new-hash-table)))))

(defun unwrap-value (value)
  (switch-mal-type value
    (list (mapcar #'unwrap-value (mal-data-value value)))
    (vector (map 'vector #'unwrap-value (mal-data-value value)))
    (hash-map (let ((hash-table (make-hash-table))
                    (hash-map-value (mal-data-value value)))
                (loop
                   for key being the hash-keys of hash-map-value
                   do (setf (gethash (mal-data-value key) hash-table)
                            (mal-data-value (gethash key hash-map-value))))
                hash-table))
    (any (mal-data-value value))))

(defun apply-unwrapped-values (op &rest values)
  (wrap-value (apply op (mapcar #'unwrap-value values))))

(defun apply-unwrapped-values-prefer-bool (op &rest values)
  (wrap-value (apply op (mapcar #'unwrap-value values)) :booleanp t))
