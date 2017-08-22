(defpackage :core
  (:use :common-lisp
        :utils
        :types
        :reader
        :printer
        :genhash
        :alexandria)
  (:export :ns))

(in-package :core)

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

(defmacro wrap-boolean (form)
  `(if ,form mal-true mal-false))

(defvar ns nil)

(defmacro defmal (name arglist &rest body)
  (let* ((symbol-name (if (stringp name)
                          name
                          ;; Since common lisp intern all the symbols in
                          ;; uppercase (by default) we need to convert the
                          ;; symbol to lowercase while introducing it in MAL
                          ;; environment
                          (string-downcase (symbol-name name))))
         (internal-name (format nil "MAL-~a" (string-upcase symbol-name))))
    `(push (cons (make-mal-symbol ,symbol-name)
                 (make-mal-builtin-fn (defun ,(intern internal-name) ,arglist ,@body)))
           ns)))

(defmal + (value1 value2)
  (apply-unwrapped-values '+ value1 value2))

(defmal - (value1 value2)
  (apply-unwrapped-values '- value1 value2))

(defmal * (value1 value2)
  (apply-unwrapped-values '* value1 value2))

(defmal / (value1 value2)
  (make-mal-number (round (/ (mal-data-value value1) (mal-data-value value2)))))

(defmal prn (&rest strings)
  ;; Using write-line instead of (format *standard-output* ... ) since the later prints
  ;; and extra newline at start in GNU CLISP, if environment variable PERL_RL is true
  ;; or terminal is dumb
  (write-line (format nil
                      "~{~a~^ ~}"
                      (mapcar (lambda (string) (printer:pr-str string t))
                              strings)))
  mal-nil)

(defmal println (&rest strings)
  ;; Using write-line instead of (format *standard-output* ... ) since the later prints
  ;; and extra newline at start in GNU CLISP, if environment variable PERL_RL is true
  ;; or terminal is dumb
  (write-line (format nil
                      "~{~a~^ ~}"
                      (mapcar (lambda (string) (printer:pr-str string nil))
                              strings)))
  mal-nil)

(defmal pr-str (&rest strings)
  (make-mal-string (format nil
                           "~{~a~^ ~}"
                           (mapcar (lambda (string) (printer:pr-str string t))
                                   strings))))

(defmal str (&rest strings)
  (make-mal-string (format nil
                           "~{~a~}"
                           (mapcar (lambda (string) (printer:pr-str string nil))
                                   strings))))

(defmal list (&rest values)
  (make-mal-list values))

(defmal list? (value)
  (wrap-boolean (or (types:mal-nil-p value) (types:mal-list-p value))))

(defmal empty? (value)
  (wrap-boolean (zerop (length (types:mal-data-value value)))))

(defmal count (value)
  (types:apply-unwrapped-values 'length value))

(defmal = (value1 value2)
  (wrap-boolean (types:mal-data-value= value1 value2)))

(defmal < (value1 value2)
  (wrap-boolean (< (mal-data-value value1) (mal-data-value value2))))

(defmal > (value1 value2)
  (wrap-boolean (> (mal-data-value value1) (mal-data-value value2))))

(defmal <= (value1 value2)
  (wrap-boolean (<= (mal-data-value value1) (mal-data-value value2))))

(defmal >= (value1 value2)
  (wrap-boolean (>= (mal-data-value value1) (mal-data-value value2))))

(defmal read-string (value)
  (reader:read-str (types:mal-data-value value)))

(defmal slurp (filename)
  (types:apply-unwrapped-values 'read-file-string filename))

(defmal atom (value)
  (types:make-mal-atom value))

(defmal atom? (value)
  (wrap-boolean (types:mal-atom-p value)))

(defmal deref (atom)
  (types:mal-data-value atom))

(defmal reset! (atom value)
  (setf (types:mal-data-value atom) value))

(defmal swap! (atom fn &rest args)
  (setf (types:mal-data-value atom)
        (apply (types:mal-data-value fn)
               (append (list (types:mal-data-value atom)) args))))

(defmal cons (element list)
  (types:make-mal-list (cons element (listify (types:mal-data-value list)))))

(defmal concat (&rest lists)
  (types:make-mal-list (apply #'concatenate 'list (mapcar #'types:mal-data-value lists))))

(defmal nth (sequence index)
  (or (nth (types:mal-data-value index)
           (listify (types:mal-data-value sequence)))
      (error 'index-error
             :size (length (types:mal-data-value sequence))
             :index (types:mal-data-value index)
             :sequence sequence)))

(defmal first (sequence)
  (or (first (listify (types:mal-data-value sequence))) mal-nil))

(defmal rest (sequence)
  (types:make-mal-list (rest (listify (types:mal-data-value sequence)))))

(defmal throw (value)
  (error 'types:mal-user-exception :data value))

(defmal apply (fn &rest values)
  (let ((last (listify (types:mal-data-value (car (last values)))))
        (butlast (butlast values)))
    (apply (types:mal-data-value fn) (append butlast last))))

(defmal map (fn sequence)
  (let ((applicants (listify (types:mal-data-value sequence))))
    (types:make-mal-list (mapcar (types:mal-data-value fn) applicants))))

(defmal nil? (value)
  (wrap-boolean (types:mal-nil-p value)))

(defmal true? (value)
  (wrap-boolean (and (types:mal-boolean-p value) (types:mal-data-value value))))

(defmal false? (value)
  (wrap-boolean (and (types:mal-boolean-p value) (not (types:mal-data-value value)))))

(defmal symbol (string)
  (types:make-mal-symbol (types:mal-data-value string)))

(defmal symbol? (value)
  (wrap-boolean (types:mal-symbol-p value)))

(defmal keyword (keyword)
  (if (types:mal-keyword-p keyword)
      keyword
      (types:make-mal-keyword (format nil ":~a" (types:mal-data-value keyword)))))

(defmal keyword? (value)
  (wrap-boolean (types:mal-keyword-p value)))

(defmal vector (&rest elements)
  (types:make-mal-vector (map 'vector #'identity elements)))

(defmal vector? (value)
  (wrap-boolean (types:mal-vector-p value)))

(defmal hash-map (&rest elements)
  (let ((hash-map (types:make-mal-value-hash-table)))
   (loop for (key value) on elements
       by #'cddr
       do (setf (genhash:hashref key hash-map) value))
    (types:make-mal-hash-map hash-map)))

(defmal map? (value)
  (wrap-boolean (types:mal-hash-map-p value)))

(defmal assoc (hash-map &rest elements)
  (let ((hash-map-value (types:mal-data-value hash-map))
        (new-hash-map (types:make-mal-value-hash-table)))

    (genhash:hashmap (lambda (key value)
                       (declare (ignorable value))
                       (setf (genhash:hashref key new-hash-map)
                             (genhash:hashref key hash-map-value)))
                     hash-map-value)

    (loop for (key value) on elements
       by #'cddr
       do (setf (genhash:hashref key new-hash-map) value))

    (types:make-mal-hash-map new-hash-map)))

(defmal dissoc (hash-map &rest elements)
  (let ((hash-map-value (mal-data-value hash-map))
        (new-hash-map (make-mal-value-hash-table)))

    (hashmap (lambda (key value)
               (declare (ignorable value))
               (when (not (member key elements :test #'mal-data-value=))
                 (setf (hashref key new-hash-map)
                       (hashref key hash-map-value))))
             hash-map-value)

    (make-mal-hash-map new-hash-map)))

(defmal get (hash-map key)
  (or (and (types:mal-hash-map-p hash-map) (genhash:hashref key (types:mal-data-value hash-map)))
      types:mal-nil))

(defmal contains? (hash-map key)
  (if (genhash:hashref key (types:mal-data-value hash-map)) types:mal-true types:mal-false))

(defmal keys (hash-map)
  (let ((hash-map-value (types:mal-data-value hash-map))
        keys)

    (hashmap (lambda (key value)
               (declare (ignorable value))
               (push key keys))
             hash-map-value)

    (make-mal-list (nreverse keys))))

(defmal vals (hash-map)
  (let ((hash-map-value (types:mal-data-value hash-map))
        values)

    (hashmap (lambda (key value)
               (declare (ignorable key))
               (push value values))
             hash-map-value)

    (make-mal-list (nreverse values))))

(defmal sequential? (value)
  (wrap-boolean (or (types:mal-vector-p value) (types:mal-list-p value))))

(defmal readline (prompt)
  (format *standard-output* (types:mal-data-value prompt))
  (force-output *standard-output*)
  (types:wrap-value (read-line *standard-input* nil)))

(defmal string? (value)
  (wrap-boolean (types:mal-string-p value)))

(defmal time-ms ()
  (types:make-mal-number (round (/ (get-internal-real-time)
                                   (/ internal-time-units-per-second
                                      1000)))))

(defmal conj (value &rest elements)
  (cond ((types:mal-list-p value)
         (types:make-mal-list (append (nreverse elements)
                                      (types:mal-data-value value))))
        ((types:mal-vector-p value)
         (types:make-mal-vector (concatenate 'vector
                                             (types:mal-data-value value)
                                             elements)))
        (t (error 'types:mal-user-exception))))

(defmal seq (value)
  (if (zerop (length (mal-data-value value)))
      mal-nil
      (cond ((mal-list-p value) value)
            ((mal-vector-p value)
             (make-mal-list (listify (mal-data-value value))))
            ((mal-string-p value)
             (make-mal-list (mapcar (alexandria:compose #'make-mal-string #'string)
                                    (coerce (mal-data-value value) 'list))))
            (t (error 'mal-user-exception)))))

(defmal with-meta (value meta)
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
           :attrs (types:mal-data-attrs value)))

(defmal meta (value)
  (or (types:mal-data-meta value) types:mal-nil))

(defun wrap-value (value &optional booleanp listp)
  (typecase value
    (number (make-mal-number value))
    ;; This needs to be before symbol since nil is a symbol
    (null (cond (booleanp mal-false)
                (listp (make-mal-list value))
                (t mal-nil)))
    ;; This needs to before symbol since t, nil are symbols
    (boolean (if value mal-true mal-nil))
    (keyword (make-mal-keyword value))
    (symbol (make-mal-symbol (symbol-name value)))
    (string (make-mal-string value))
    (list (make-mal-list (map 'list #'wrap-value value)))
    (vector (make-mal-vector (map 'vector #'wrap-value value)))
    (hash-table (make-mal-hash-map (let ((new-hash-table (make-mal-value-hash-table)))
                                     (hashmap (lambda (key value)
                                                (setf (hashref (wrap-value key) new-hash-table)
                                                      (wrap-value value)))
                                              value)
                                     new-hash-table)))))

;; Since a nil in Common LISP may mean an empty list or boolean false or
;; simply nil, the caller can specify the preferred type while evaluating an
;; expression
(defmal cl-eval (code &optional booleanp listp)
  (types:wrap-value (eval (read-from-string (types:mal-data-value code)))
                    :booleanp (and booleanp (types:mal-data-value booleanp))
                    :listp (and listp (types:mal-data-value listp))))
