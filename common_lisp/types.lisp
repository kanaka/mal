(defpackage :types
  (:use :common-lisp)
  (:export :mal-value=
           ;; Accessors
           :mal-value
           :mal-type
           :mal-meta
           :mal-attrs
           ;; Mal values
           :number
           :boolean
           :nil
           :string
           :symbol
           :keyword
           :list
           :vector
           :hash-map
           :fn
           :builtin-fn
           :any
           ;; Helpers
           :wrap-value
           :apply-unwrapped-values
           :apply-unwrapped-values-prefer-bool
           :switch-mal-type))

(in-package :types)

(defclass mal-type ()
  ((value :accessor mal-value :initarg :value)
   (meta :accessor mal-meta :initarg :meta :initform nil)
   (type :accessor mal-type :initarg :type)
   (attrs :accessor mal-attrs :initarg :attrs)))

(defmethod print-object ((obj mal-type) out)
  (with-slots (value type meta attrs) obj
    (format out "#<mal ~a: ~a (~a, ~a)>" type value meta attrs)))

(defmacro define-mal-type (type)
  ;; Create a class for given type and a convenience constructor and also export
  ;; them
  (let ((name (intern (string-upcase (concatenate 'string
                                                  "mal-"
                                                  (symbol-name type)))))
        (constructor (intern (string-upcase (concatenate 'string
                                                         "make-mal-"
                                                         (symbol-name type)))))
        (predicate (intern (string-upcase (concatenate 'string
                                                         "mal-"
                                                         (symbol-name type)
                                                         "-p")))))
    `(progn (defclass ,name (mal-type)
              ((type :accessor mal-type
                     :initarg :type
                     :initform ',type)))

            (defun ,constructor (value &key meta attrs)
              (make-instance ',name
                             :value value
                             :meta meta
                             :attrs attrs))
            (defun ,predicate (value)
              (when (typep value 'mal-type)
                (equal (mal-type value) ',type)))

            (export ',name)
            (export ',constructor)
            (export ',predicate))))

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

(define-mal-type fn)
(define-mal-type builtin-fn)

;; Generic type
(defvar any)

(defmacro switch-mal-type (ast &body forms)
  `(let ((type (types:mal-type ,ast)))
     (cond
       ,@(mapcar (lambda (form)
                   (list (if (or (equal (car form) t)
                                 (equal (car form) 'any))
                             t
                             (list 'equal (list 'quote (car form)) 'type))
                         (cadr form)))
                 forms))))

(defun mal-symbol= (value1 value2)
  (string= (symbol-name (mal-value value1))
           (symbol-name (mal-value value2))))

(defun mal-sequence= (value1 value2)
  (let ((sequence1 (map 'list #'identity (mal-value value1)))
        (sequence2 (map 'list #'identity (mal-value value2))))
    (when (= (length sequence1) (length sequence2))
      (every #'identity
             (loop
                for x in sequence1
                for y in sequence2
                collect (mal-value= x y))))))

(defun mal-hash-map= (value1 value2)
  (let ((map1 (mal-value value1))
        (map2 (mal-value value2)))
    (when (= (hash-table-count map1) (hash-table-count map2))
      (every #'identity
             (loop
                for key being the hash-keys of map1
                collect (mal-value= (gethash key map1)
                                    (gethash key map2)))))))

(defun mal-value= (value1 value2)
  (when (and (typep value1 'mal-type)
             (typep value2 'mal-type))
    (if (equal (mal-type value1) (mal-type value2))
      (switch-mal-type value1
        (number (= (mal-value value1) (mal-value value2)))
        (boolean (equal (mal-value value1) (mal-value value2)))
        (nil (equal (mal-value value1) (mal-value value2)))
        (string (string= (mal-value value1) (mal-value value2)))
        (symbol (mal-symbol= value1 value2))
        (keyword (mal-symbol= value1 value2))
        (list (mal-sequence= value1 value2))
        (vector (mal-sequence= value1 value2))
        (hash-map (mal-hash-map= value1 value2))
        (any nil))
      (when (or (and (mal-list-p value1) (mal-vector-p value2))
                (and (mal-list-p value2) (mal-vector-p value1)))
        (mal-sequence= value1 value2)))))

(defun hash-mal-value (value)
  (sxhash (mal-value value)))

#+sbcl (sb-ext:define-hash-table-test mal-value= hash-mal-value)
#+clisp (ext:define-hash-table-test mal-value= mal-value= hash-mal-value)

(defun wrap-value (value &key booleanp)
  (funcall (typecase value
             (number #'make-mal-number)
             ;; This needs to before symbol since nil is a symbol
             (null (if booleanp
                       #'make-mal-boolean
                       #'make-mal-nil))
             ;; This needs to before symbol since t, nil are symbols
             (boolean #'make-mal-boolean)
             (symbol #'make-mal-symbol)
             (keyword #'make-mal-keyword)
             (string #'make-mal-string)
             (list #'make-mal-list)
             (vector #'make-mal-vector)
             (hash-table #'make-mal-hash-map)
             (null #'make-mal-nil))
           value))

(defun apply-unwrapped-values (op &rest values)
  (wrap-value (apply op (mapcar #'mal-value values))))

(defun apply-unwrapped-values-prefer-bool (op &rest values)
  (wrap-value (apply op (mapcar #'mal-value values)) :booleanp t))
