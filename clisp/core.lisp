(defpackage :core
  (:use :common-lisp :types :reader :printer)
  (:export :ns))

(in-package :core)

(define-condition index-error (types:mal-runtime-exception)
  ((size :initarg :size :reader size)
   (index :initarg :index :reader index)
   (sequence :initarg :sequence :reader sequence))
  (:report (lambda (condition stream)
             (format stream
                     "Index out of range (~a), length is ~a but index given was ~a"
                     (printer:pr-str (sequence condition))
                     (size condition)
                     (index condition)))))

(defun get-file-contents (filename)
  (with-open-file (stream filename)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defmacro wrap-boolean (form)
  `(if ,form
       types:mal-true
       types:mal-false))

(defvar ns
  (list
   (cons (types:make-mal-symbol "+")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values '+ value1 value2))))

   (cons (types:make-mal-symbol "-")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values '- value1 value2))))

   (cons (types:make-mal-symbol "*")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values '* value1 value2))))

   (cons (types:make-mal-symbol "/")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:make-mal-number (float (/ (types:mal-data-value value1)
                                                                       (types:mal-data-value value2)))))))

   (cons (types:make-mal-symbol "prn")
         (types:make-mal-builtin-fn (lambda (&rest strings)
                                      (write-line (format nil
                                                          "~{~a~^ ~}"
                                                          (mapcar (lambda (string) (printer:pr-str string t))
                                                                  strings)))
                                      types:mal-nil)))

   (cons (types:make-mal-symbol "println")
         (types:make-mal-builtin-fn (lambda (&rest strings)
                                      (write-line (format nil
                                                          "~{~a~^ ~}"
                                                          (mapcar (lambda (string) (printer:pr-str string nil))
                                                                  strings)))
                                      types:mal-nil)))

   (cons (types:make-mal-symbol "pr-str")
         (types:make-mal-builtin-fn (lambda (&rest strings)
                                      (types:make-mal-string (format nil
                                                                     "~{~a~^ ~}"
                                                                     (mapcar (lambda (string) (printer:pr-str string t))
                                                                             strings))))))

   (cons (types:make-mal-symbol "str")
         (types:make-mal-builtin-fn (lambda (&rest strings)
                                      (types:make-mal-string (format nil
                                                                     "~{~a~}"
                                                                     (mapcar (lambda (string) (printer:pr-str string nil))
                                                                             strings))))))

   (cons (types:make-mal-symbol "list")
         (types:make-mal-builtin-fn (lambda (&rest values)
                                      (make-mal-list values))))

   (cons (types:make-mal-symbol "list?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (or (types:mal-nil-p value)
                                                        (types:mal-list-p value))))))

   (cons (types:make-mal-symbol "empty?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (zerop (length (mal-data-value value)))))))

   (cons (types:make-mal-symbol "count")
         (types:make-mal-builtin-fn (lambda (value)
                                      (types:apply-unwrapped-values 'length value))))

   (cons (types:make-mal-symbol "=")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (wrap-boolean (types:mal-value= value1 value2)))))

   (cons (types:make-mal-symbol "<")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values-prefer-bool '<
                                                                                value1
                                                                                value2))))

   (cons (types:make-mal-symbol ">")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values-prefer-bool '>
                                                                                value1
                                                                                value2))))

   (cons (types:make-mal-symbol "<=")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values-prefer-bool '<=
                                                                                value1
                                                                                value2))))

   (cons (types:make-mal-symbol ">=")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (types:apply-unwrapped-values-prefer-bool '>=
                                                                                value1
                                                                                value2))))

   (cons (types:make-mal-symbol "read-string")
         (types:make-mal-builtin-fn (lambda (value)
                                      (reader:read-str (types:mal-data-value value)))))

   (cons (types:make-mal-symbol "slurp")
         (types:make-mal-builtin-fn (lambda (filename)
                                      (types:apply-unwrapped-values 'get-file-contents filename))))

   (cons (types:make-mal-symbol "atom")
         (types:make-mal-builtin-fn (lambda (value)
                                      (types:make-mal-atom value))))

   (cons (types:make-mal-symbol "atom?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (types:mal-atom-p value)))))

   (cons (types:make-mal-symbol "deref")
         (types:make-mal-builtin-fn (lambda (atom)
                                      (types:mal-data-value atom))))

   (cons (types:make-mal-symbol "reset!")
         (types:make-mal-builtin-fn (lambda (atom value)
                                      (setf (types:mal-data-value atom) value))))

   (cons (types:make-mal-symbol "swap!")
         (types:make-mal-builtin-fn (lambda (atom fn &rest args)
                                      (setf (types:mal-data-value atom)
                                            (apply (mal-data-value fn)
                                                   (append (list (types:mal-data-value atom))
                                                           args))))))

   (cons (types:make-mal-symbol "cons")
         (types:make-mal-builtin-fn (lambda (element list)
                                      (types:make-mal-list (cons element
                                                                 (map 'list
                                                                      #'identity
                                                                      (mal-data-value list)))))))

   (cons (types:make-mal-symbol "concat")
         (types:make-mal-builtin-fn (lambda (&rest lists)
                                      (types:make-mal-list (apply #'concatenate
                                                                  'list
                                                                  (mapcar #'types:mal-data-value lists))))))


   (cons (types:make-mal-symbol "nth")
         (types:make-mal-builtin-fn (lambda (sequence index)
                                      (or (nth (mal-data-value index)
                                               (map 'list #'identity (mal-data-value sequence)))
                                          (error 'index-error
                                                 :size (length (mal-data-value sequence))
                                                 :index (mal-data-value index)
                                                 :sequence sequence)))))

   (cons (types:make-mal-symbol "first")
         (types:make-mal-builtin-fn (lambda (sequence)
                                      (or (first (map 'list #'identity (mal-data-value sequence)))
                                          types:mal-nil))))

   (cons (types:make-mal-symbol "rest")
         (types:make-mal-builtin-fn (lambda (sequence)
                                      (types:make-mal-list (rest (map 'list
                                                                      #'identity
                                                                      (mal-data-value sequence)))))))

   (cons (types:make-mal-symbol "throw")
         (types:make-mal-builtin-fn (lambda (value)
                                      (error 'types:mal-user-exception
                                             :data value))))

   (cons (types:make-mal-symbol "apply")
         (types:make-mal-builtin-fn (lambda (fn &rest values)
                                      (let ((final-arg (map 'list
                                                            #'identity
                                                            (types:mal-data-value (car (last values)))))
                                            (butlast-args (butlast values)))
                                        (apply (types:mal-data-value fn)
                                               (append butlast-args final-arg))))))

   (cons (types:make-mal-symbol "map")
         (types:make-mal-builtin-fn (lambda (fn sequence)
                                      (let ((applicants (map 'list
                                                             #'identity
                                                             (types:mal-data-value sequence))))
                                        (types:make-mal-list (mapcar (types:mal-data-value fn)
                                                                     applicants))))))

   (cons (types:make-mal-symbol "nil?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (types:mal-nil-p value)))))

   (cons (types:make-mal-symbol "true?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (and (types:mal-boolean-p value)
                                                                   (types:mal-data-value value))))))

   (cons (types:make-mal-symbol "false?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (and (types:mal-boolean-p value)
                                                                   (not (types:mal-data-value value)))))))

   (cons (types:make-mal-symbol "symbol?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (types:mal-symbol-p value)))))

   (cons (types:make-mal-symbol "symbol")
         (types:make-mal-builtin-fn (lambda (string)
                                      (types:make-mal-symbol (types:mal-data-value string)))))

   (cons (types:make-mal-symbol "keyword")
         (types:make-mal-builtin-fn (lambda (keyword)
                                      (if (types:mal-keyword-p keyword)
                                          keyword
                                          (types:make-mal-keyword (format nil ":~a" (types:mal-data-value keyword)))))))

   (cons (types:make-mal-symbol "keyword?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (types:mal-keyword-p value)))))

   (cons (types:make-mal-symbol "vector")
         (types:make-mal-builtin-fn (lambda (&rest elements)
                                      (types:make-mal-vector (map 'vector #'identity elements)))))

   (cons (types:make-mal-symbol "vector?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (types:mal-vector-p value)))))

   (cons (types:make-mal-symbol "hash-map")
         (types:make-mal-builtin-fn (lambda (&rest elements)
                                      (let ((hash-map (make-hash-table :test 'types:mal-value=)))
                                        (loop
                                           for (key value) on elements
                                           by #'cddr
                                           do (setf (gethash key hash-map) value))
                                        (types:make-mal-hash-map hash-map)))))

   (cons (types:make-mal-symbol "map?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (types:mal-hash-map-p value)))))

   (cons (types:make-mal-symbol "assoc")
         (types:make-mal-builtin-fn (lambda (hash-map &rest elements)
                                      (let ((hash-map-value (types:mal-data-value hash-map))
                                            (new-hash-map (make-hash-table :test 'types:mal-value=)))

                                        (loop
                                           for key being the hash-keys of hash-map-value
                                           do (setf (gethash key new-hash-map)
                                                    (gethash key hash-map-value)))

                                        (loop
                                           for (key value) on elements
                                           by #'cddr
                                           do (setf (gethash key new-hash-map) value))

                                        (types:make-mal-hash-map new-hash-map)))))

   (cons (types:make-mal-symbol "dissoc")
         (types:make-mal-builtin-fn (lambda (hash-map &rest elements)
                                      (let ((hash-map-value (types:mal-data-value hash-map))
                                            (new-hash-map (make-hash-table :test 'types:mal-value=)))

                                        (loop
                                           for key being the hash-keys of hash-map-value
                                           do (when (not (member key elements :test #'types:mal-value=))
                                                (setf (gethash key new-hash-map)
                                                      (gethash key hash-map-value))))

                                        (types:make-mal-hash-map new-hash-map)))))

   (cons (types:make-mal-symbol "get")
         (types:make-mal-builtin-fn (lambda (hash-map key)
                                      (or (and (types:mal-hash-map-p hash-map)
                                               (gethash key (types:mal-data-value hash-map)))
                                          types:mal-nil))))

   (cons (types:make-mal-symbol "contains?")
         (types:make-mal-builtin-fn (lambda (hash-map key)
                                      (if (gethash key (types:mal-data-value hash-map))
                                          types:mal-true
                                          types:mal-false))))

   (cons (types:make-mal-symbol "keys")
         (types:make-mal-builtin-fn (lambda (hash-map)
                                      (let ((hash-map-value (types:mal-data-value hash-map)))
                                        (types:make-mal-list (loop
                                                                for key being the hash-keys of hash-map-value
                                                                collect key))))))

   (cons (types:make-mal-symbol "vals")
         (types:make-mal-builtin-fn (lambda (hash-map)
                                      (let ((hash-map-value (types:mal-data-value hash-map)))
                                        (types:make-mal-list (loop
                                                                for key being the hash-keys of hash-map-value
                                                                collect (gethash key hash-map-value)))))))

   (cons (types:make-mal-symbol "sequential?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (or (types:mal-vector-p value)
                                                        (types:mal-list-p value))))))

   (cons (types:make-mal-symbol "readline")
         (types:make-mal-builtin-fn (lambda (prompt)
                                      (format *standard-output* (types:mal-data-value prompt))
                                      (force-output *standard-output*)
                                      (types:wrap-value (read-line *standard-input* nil)))))

   (cons (types:make-mal-symbol "string?")
         (types:make-mal-builtin-fn (lambda (value)
                                      (wrap-boolean (types:mal-string-p value)))))

   (cons (types:make-mal-symbol "time-ms")
         (types:make-mal-builtin-fn (lambda ()

                                      (types:make-mal-number (floor (/ (get-internal-real-time)
                                                                       (/ internal-time-units-per-second
                                                                          1000)))))))

   (cons (types:make-mal-symbol "conj")
         (types:make-mal-builtin-fn (lambda (value &rest elements)
                                      (cond ((types:mal-list-p value)
                                             (types:make-mal-list (append (nreverse elements)
                                                                          (types:mal-data-value value))))
                                            ((types:mal-vector-p value)
                                             (types:make-mal-vector (concatenate 'vector
                                                                                 (types:mal-data-value value)
                                                                                 elements)))
                                            (t (error 'types:mal-user-exception))))))
   (cons (types:make-mal-symbol "seq")
         (types:make-mal-builtin-fn (lambda (value)
                                      (if (zerop (length (types:mal-data-value value)))
                                          types:mal-nil
                                          (cond ((types:mal-list-p value)
                                                 value)
                                                ((types:mal-vector-p value)
                                                 (types:make-mal-list (map 'list
                                                                           #'identity
                                                                           (types:mal-data-value value))))
                                                ((types:mal-string-p value)
                                                 (types:make-mal-list  (map 'list
                                                                            (lambda (char)
                                                                              (types:make-mal-string (make-string 1 :initial-element char)))
                                                                            (types:mal-data-value value))))
                                                (t (error 'types:mal-user-exception)))))))

   (cons (types:make-mal-symbol "with-meta")
         (types:make-mal-builtin-fn (lambda (value meta)
                                      (funcall (switch-mal-type value
                                                 (types:string #'types:make-mal-string)
                                                 (types:symbol #'types:make-mal-symbol)
                                                 (types:list #'types:make-mal-list)
                                                 (types:vector #'types:make-mal-vector)
                                                 (types:hash-map #'types:make-mal-hash-map)
                                                 (types:fn #'types:make-mal-fn)
                                                 (types:builtin-fn #'types:make-mal-builtin-fn))
                                               (types:mal-data-value value)
                                               :meta meta
                                               :attrs (types:mal-data-attrs value)))))

   (cons (types:make-mal-symbol "meta")
         (types:make-mal-builtin-fn (lambda (value)
                                      (or (types:mal-data-meta value)
                                          types:mal-nil))))

   ;; Since a nil in clisp may mean an empty list or boolean false or simply nil, the
   ;; caller can specify the preferred type while evaluating an expression
   (cons (types:make-mal-symbol "clisp-eval")
         (types:make-mal-builtin-fn (lambda (code &optional booleanp listp)
                                      (types:wrap-value (eval (read-from-string (types:mal-data-value code)))
                                                        :booleanp (and booleanp (types:mal-data-value booleanp))
                                                        :listp (and listp (types:mal-data-value listp))))))

   (cons (types:make-mal-symbol "define-builtin")
         (types:make-mal-builtin-fn (lambda (arglist &rest body)
                                      (let* ((func-args (types:unwrap-value arglist))
                                             (func-body (mapcar #'types:unwrap-value body))
                                             (func (eval `(lambda ,func-args ,@func-body))))
                                        (types:make-mal-builtin-fn (lambda (&rest args)
                                                                     (types:wrap-value (apply func
                                                                                              (mapcar #'types:unwrap-value args)))))))))))
