(defpackage :core
  (:use :common-lisp
        :utils
        :types
        :reader
        :printer
        :genhash)
  (:export :ns))

(in-package :core)

(defmacro wrap-boolean (form)
  `(if ,form
       types:mal-true
       types:mal-false))

(define-condition index-error (types:mal-error)
  ((size :initarg :size :reader index-error-size)
   (index :initarg :index :reader index-error-index)
   (sequence :initarg :sequence :reader index-error-sequence))
  (:report (lambda (condition stream)
             (format stream
                     "Index out of range (~a), length is ~a but index given was ~a"
                     (printer:pr-str (index-error-sequence condition))
                     (index-error-size condition)
                     (index-error-index condition)))))

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

(defun mal-nth (sequence index)
  (or (nth (types:mal-data-value index)
           (map 'list #'identity (types:mal-data-value sequence)))
      (error 'index-error
             :size (length (mal-value sequence))
             :index (mal-value index)
             :sequence sequence)))

(defun mal-first (sequence)
  (or (first (map 'list #'identity (types:mal-data-value sequence)))
      (types:make-mal-nil nil)))

(defun mal-rest (sequence)
  (types:make-mal-list (rest (map 'list
                                  #'identity
                                  (types:mal-data-value sequence)))))

(defun mal-throw (value)
  (error 'types:mal-user-exception
         :data value))

(defun mal-apply (fn &rest values)
  (let ((final-arg (map 'list
                        #'identity
                        (types:mal-data-value (car (last values)))))
        (butlast-args (butlast values)))
    (apply (types:mal-data-value fn)
           (append butlast-args final-arg))))

(defun mal-map (fn sequence)
  (let ((applicants (map 'list
                         #'identity
                         (types:mal-data-value sequence))))
    (types:make-mal-list (mapcar (types:mal-data-value fn)
                                 applicants))))

(defun mal-nil? (value)
  (wrap-boolean (types:mal-nil-p value)))

(defun mal-true? (value)
  (wrap-boolean (and (types:mal-boolean-p value)
                     (types:mal-data-value value))))

(defun mal-false? (value)
  (wrap-boolean (and (types:mal-boolean-p value)
                     (not (types:mal-data-value value)))))

(defun mal-symbol (string)
  (types:make-mal-symbol (types:mal-data-value string)))

(defun mal-symbol? (value)
  (wrap-boolean (types:mal-symbol-p value)))

(defun mal-keyword (keyword)
  (if (types:mal-keyword-p keyword)
      keyword
      (types:make-mal-keyword (format nil ":~a" (types:mal-data-value keyword)))))

(defun mal-keyword? (value)
  (wrap-boolean (types:mal-keyword-p value)))

(defun mal-vector (&rest elements)
  (types:make-mal-vector (map 'vector #'identity elements)))

(defun mal-vector? (value)
  (wrap-boolean (types:mal-vector-p value)))

(defun mal-hash-map (&rest elements)
  (let ((hash-map (types:make-mal-value-hash-table)))
    (loop
       for (key value) on elements
       by #'cddr
       do (setf (genhash:hashref key hash-map) value))
    (types:make-mal-hash-map hash-map)))

(defun mal-map? (value)
  (wrap-boolean (types:mal-hash-map-p value)))

(defun mal-assoc (hash-map &rest elements)
  (let ((hash-map-value (types:mal-data-value hash-map))
        (new-hash-map (types:make-mal-value-hash-table)))

    (genhash:hashmap (lambda (key value)
                       (declare (ignorable value))
                       (setf (genhash:hashref key new-hash-map)
                             (genhash:hashref key hash-map-value)))
                     hash-map-value)

    (loop
       for (key value) on elements
       by #'cddr
       do (setf (genhash:hashref key new-hash-map) value))

    (types:make-mal-hash-map new-hash-map)))

(defun mal-dissoc (hash-map &rest elements)
  (let ((hash-map-value (types:mal-data-value hash-map))
        (new-hash-map (types:make-mal-value-hash-table)))

    (genhash:hashmap (lambda (key value)
                       (declare (ignorable value))
                       (when (not (member key
                                          elements
                                          :test #'types:mal-data-value=))
                         (setf (genhash:hashref key new-hash-map)
                               (genhash:hashref key hash-map-value))))
                     hash-map-value)
    (types:make-mal-hash-map new-hash-map)))

(defun mal-get (hash-map key)
  (or (and (types:mal-hash-map-p hash-map)
           (genhash:hashref key (types:mal-data-value hash-map)))
      types:mal-nil))

(defun mal-contains? (hash-map key)
  (if (genhash:hashref key (types:mal-data-value hash-map))
      types:mal-true
      types:mal-false))

(defun mal-keys (hash-map)
  (let ((hash-map-value (types:mal-data-value hash-map))
        keys)
    (genhash:hashmap (lambda (key value)
                       (declare (ignorable value))
                       (push key keys))
                     hash-map-value)
    (types:make-mal-list (nreverse keys))))

(defun mal-vals (hash-map)
  (let ((hash-map-value (types:mal-data-value hash-map))
        values)
    (genhash:hashmap (lambda (key value)
                       (declare (ignorable key))
                       (push value values))
                     hash-map-value)
    (types:make-mal-list (nreverse values))))

(defun mal-sequential? (value)
  (wrap-boolean (or (types:mal-vector-p value)
                    (types:mal-list-p value))))

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
   (cons (types:make-mal-symbol "concat") (types:make-mal-builtin-fn #'mal-concat))
   (cons (types:make-mal-symbol "nth") (types:make-mal-builtin-fn #'mal-nth))
   (cons (types:make-mal-symbol "first") (types:make-mal-builtin-fn #'mal-first))
   (cons (types:make-mal-symbol "rest") (types:make-mal-builtin-fn #'mal-rest))
   (cons (types:make-mal-symbol "throw") (types:make-mal-builtin-fn #'mal-throw))
   (cons (types:make-mal-symbol "apply") (types:make-mal-builtin-fn #'mal-apply))
   (cons (types:make-mal-symbol "map") (types:make-mal-builtin-fn #'mal-map))
   (cons (types:make-mal-symbol "nil?") (types:make-mal-builtin-fn #'mal-nil?))
   (cons (types:make-mal-symbol "true?") (types:make-mal-builtin-fn #'mal-true?))
   (cons (types:make-mal-symbol "false?") (types:make-mal-builtin-fn #'mal-false?))
   (cons (types:make-mal-symbol "symbol") (types:make-mal-builtin-fn #'mal-symbol))
   (cons (types:make-mal-symbol "symbol?") (types:make-mal-builtin-fn #'mal-symbol?))
   (cons (types:make-mal-symbol "keyword") (types:make-mal-builtin-fn #'mal-keyword))
   (cons (types:make-mal-symbol "keyword?") (types:make-mal-builtin-fn #'mal-keyword?))
   (cons (types:make-mal-symbol "vector") (types:make-mal-builtin-fn #'mal-vector))
   (cons (types:make-mal-symbol "vector?") (types:make-mal-builtin-fn #'mal-vector?))
   (cons (types:make-mal-symbol "hash-map") (types:make-mal-builtin-fn #'mal-hash-map))
   (cons (types:make-mal-symbol "map?") (types:make-mal-builtin-fn #'mal-map?))
   (cons (types:make-mal-symbol "assoc") (types:make-mal-builtin-fn #'mal-assoc))
   (cons (types:make-mal-symbol "dissoc") (types:make-mal-builtin-fn #'mal-dissoc))
   (cons (types:make-mal-symbol "get") (types:make-mal-builtin-fn #'mal-get))
   (cons (types:make-mal-symbol "contains?") (types:make-mal-builtin-fn #'mal-contains?))
   (cons (types:make-mal-symbol "keys") (types:make-mal-builtin-fn #'mal-keys))
   (cons (types:make-mal-symbol "vals") (types:make-mal-builtin-fn #'mal-vals))
   (cons (types:make-mal-symbol "sequential?") (types:make-mal-builtin-fn #'mal-sequential?))))
