;; Dummy package where MAL variables are interned
(defpackage :mal-user
  (:use :common-lisp))

(defpackage :types
  (:use :common-lisp)
  (:export :mal-value=

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

           :fn
           :make-mal-fn
           :mal-fn-p

           :builtin-fn
           :make-mal-builtin-fn
           :mal-builtin-fn-p

           :any

           ;; Singleton values
           :mal-nil
           :mal-true
           :mal-false

           :mal-exception

           ;; User exceptions
           :mal-user-exception

           ;; Exceptions raised by the runtime itself
           :mal-runtime-exception

           ;; Error
           :mal-error

           ;; Helpers
           :wrap-value
           :unwrap-value
           :apply-unwrapped-values
           :apply-unwrapped-values-prefer-bool
           :switch-mal-type))

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
  (type nil :read-only t)
  (value nil)
  meta
  attrs)

(defmacro define-mal-type (type)
  ;; Create a class for given type and a convenience constructor and also export
  ;; them
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
                (equal (mal-data-type value) ',type))))))

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

(define-mal-type fn)
(define-mal-type builtin-fn)

(defvar mal-nil (make-mal-nil nil))
(defvar mal-true (make-mal-boolean t))
(defvar mal-false (make-mal-boolean nil))

;; Generic type
(defvar any)

(defmacro switch-mal-type (ast &body forms)
  `(let ((type (types:mal-data-type ,ast)))
     (cond
       ,@(mapcar (lambda (form)
                   (list (if (or (equal (car form) t)
                                 (equal (car form) 'any))
                             t
                             (list 'equal (list 'quote (car form)) 'type))
                         (cadr form)))
                 forms))))

(defun mal-symbol= (value1 value2)
  (string= (mal-data-value value1)
           (mal-data-value value2)))

(defun mal-sequence= (value1 value2)
  (let ((sequence1 (map 'list #'identity (mal-data-value value1)))
        (sequence2 (map 'list #'identity (mal-data-value value2))))
    (when (= (length sequence1) (length sequence2))
      (every #'identity
             (loop
                for x in sequence1
                for y in sequence2
                collect (mal-value= x y))))))

(defun mal-hash-map= (value1 value2)
  (let ((map1 (mal-data-value value1))
        (map2 (mal-data-value value2)))
    (when (= (hash-table-count map1) (hash-table-count map2))
      (every #'identity
             (loop
                for key being the hash-keys of map1
                collect (mal-value= (gethash key map1)
                                    (gethash key map2)))))))

(defun mal-value= (value1 value2)
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

(defun hash-mal-value (value)
  (sxhash (mal-data-value value)))

(ext:define-hash-table-test mal-value= mal-value= hash-mal-value)

(defun wrap-hash-value (value)
  (let ((new-hash-table (make-hash-table :test 'mal-value=)))
    (loop
       for key being the hash-keys of value
       do (setf (gethash (wrap-value key) new-hash-table)
                (wrap-value (gethash key value))))
    new-hash-table))

(defun wrap-value (value &key booleanp listp)
  "Convert a Common Lisp value to MAL value"
  (typecase value
    (number (make-mal-number value))
    ;; This needs to before symbol since nil is a symbol
    (null (cond
            (booleanp mal-false)
            (listp (make-mal-list nil))
            (t mal-nil)))
    ;; This needs to before symbol since t, nil are symbols
    (boolean (if value mal-true mal-false))
    (symbol (make-mal-symbol (symbol-name value)))
    (keyword (make-mal-keyword value))
    (string (make-mal-string value))
    (list (make-mal-list (map 'list #'wrap-value value)))
    (vector (make-mal-vector (map 'vector #'wrap-value value)))
    (hash-table (make-mal-hash-map (wrap-hash-value value)))
    (null mal-nil)))

(defun unwrap-value (value)
  "Convert a MAL value to native Common Lisp value"
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
    ;; Unfortunately below means even symbols that user indented to use
    ;; from the common lisp are interned in lowercase thus runtime
    ;; will not find them as such users need to explicitly upcase the
    ;; symbols from common lisp
    (symbol (intern (mal-data-value value) :mal-user))
    ;; In case of a keyword strip the first colon, and intern the symbol in
    ;; keyword package
    (keyword (intern (string-upcase (subseq (mal-data-value value) 1))
                     :keyword))
    (any (mal-data-value value))))

(defun apply-unwrapped-values (op &rest values)
  (wrap-value (apply op (mapcar #'unwrap-value values))))

(defun apply-unwrapped-values-prefer-bool (op &rest values)
  (wrap-value (apply op (mapcar #'unwrap-value values)) :booleanp t))
