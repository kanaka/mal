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

(define-condition index-error (mal-error)
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
  (make-mal-number (+ (mal-data-value value1) (mal-data-value value2))))

(defmal - (value1 value2)
  (make-mal-number (- (mal-data-value value1) (mal-data-value value2))))

(defmal * (value1 value2)
  (make-mal-number (* (mal-data-value value1) (mal-data-value value2))))

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
  (wrap-boolean (or (mal-nil-p value) (mal-list-p value))))

(defmal empty? (value)
  (wrap-boolean (zerop (length (mal-data-value value)))))

(defmal count (value)
  (make-mal-number (length (mal-data-value value))))

(defmal = (value1 value2)
  (wrap-boolean (mal-data-value= value1 value2)))

(defmal < (value1 value2)
  (wrap-boolean (< (mal-data-value value1) (mal-data-value value2))))

(defmal > (value1 value2)
  (wrap-boolean (> (mal-data-value value1) (mal-data-value value2))))

(defmal <= (value1 value2)
  (wrap-boolean (<= (mal-data-value value1) (mal-data-value value2))))

(defmal >= (value1 value2)
  (wrap-boolean (>= (mal-data-value value1) (mal-data-value value2))))

(defmal read-string (value)
  (reader:read-str (mal-data-value value)))

(defmal slurp (filename)
  (make-mal-string (read-file-string (mal-data-value filename))))

(defmal atom (value)
  (make-mal-atom value))

(defmal atom? (value)
  (wrap-boolean (mal-atom-p value)))

(defmal deref (atom)
  (mal-data-value atom))

(defmal reset! (atom value)
  (setf (mal-data-value atom) value))

(defmal swap! (atom fn &rest args)
  (setf (mal-data-value atom)
        (apply (mal-data-value fn)
               (append (list (mal-data-value atom)) args))))

(defmal vec (list)
  (make-mal-vector (listify (mal-data-value list))))

(defmal cons (element list)
  (make-mal-list (cons element (listify (mal-data-value list)))))

(defmal concat (&rest lists)
  (make-mal-list (apply #'concatenate 'list (mapcar #'mal-data-value lists))))

(defmal nth (sequence index)
  (or (nth (mal-data-value index)
           (listify (mal-data-value sequence)))
      (error 'index-error
             :size (length (mal-data-value sequence))
             :index (mal-data-value index)
             :sequence sequence)))

(defmal first (sequence)
  (or (first (listify (mal-data-value sequence))) mal-nil))

(defmal rest (sequence)
  (make-mal-list (rest (listify (mal-data-value sequence)))))

(defmal throw (value)
  (error 'mal-user-exception :data value))

(defmal apply (fn &rest values)
  (let ((last (listify (mal-data-value (car (last values)))))
        (butlast (butlast values)))
    (apply (mal-data-value fn) (append butlast last))))

(defmal map (fn sequence)
  (let ((applicants (listify (mal-data-value sequence))))
    (make-mal-list (mapcar (mal-data-value fn) applicants))))

(defmal nil? (value)
  (wrap-boolean (mal-nil-p value)))

(defmal true? (value)
  (wrap-boolean (and (mal-boolean-p value) (mal-data-value value))))

(defmal false? (value)
  (wrap-boolean (and (mal-boolean-p value) (not (mal-data-value value)))))

(defmal number? (value)
  (wrap-boolean (mal-number-p value)))

(defmal symbol (string)
  (make-mal-symbol (mal-data-value string)))

(defmal symbol? (value)
  (wrap-boolean (mal-symbol-p value)))

(defmal keyword (keyword)
  (if (mal-keyword-p keyword)
      keyword
      (make-mal-keyword (format nil ":~a" (mal-data-value keyword)))))

(defmal keyword? (value)
  (wrap-boolean (mal-keyword-p value)))

(defmal vector (&rest elements)
  (make-mal-vector (map 'vector #'identity elements)))

(defmal vector? (value)
  (wrap-boolean (mal-vector-p value)))

(defmal fn? (value)
  (wrap-boolean (or (mal-builtin-fn-p value)
                    (and (mal-fn-p value)
                         (not (cdr (assoc :is-macro (mal-data-attrs value))))))))

(defmal macro? (value)
  (wrap-boolean (and (mal-fn-p value)
                     (cdr (assoc :is-macro (mal-data-attrs value))))))

(defmal hash-map (&rest elements)
  (let ((hash-map (make-mal-value-hash-table)))
   (loop for (key value) on elements
       by #'cddr
       do (setf (hashref key hash-map) value))
    (make-mal-hash-map hash-map)))

(defmal map? (value)
  (wrap-boolean (mal-hash-map-p value)))

(defmal assoc (hash-map &rest elements)
  (let ((hash-map-value (mal-data-value hash-map))
        (new-hash-map (make-mal-value-hash-table)))

    (hashmap (lambda (key value)
               (declare (ignorable value))
               (setf (hashref key new-hash-map)
                     (hashref key hash-map-value)))
             hash-map-value)

    (loop for (key value) on elements
       by #'cddr
       do (setf (hashref key new-hash-map) value))

    (make-mal-hash-map new-hash-map)))

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
  (or (and (mal-hash-map-p hash-map) (hashref key (mal-data-value hash-map)))
      types:mal-nil))

(defmal contains? (hash-map key)
  (if (genhash:hashref key (types:mal-data-value hash-map)) types:mal-true types:mal-false))

(defmal keys (hash-map)
  (let ((hash-map-value (mal-data-value hash-map))
        keys)

    (hashmap (lambda (key value)
               (declare (ignorable value))
               (push key keys))
             hash-map-value)

    (make-mal-list (nreverse keys))))

(defmal vals (hash-map)
  (let ((hash-map-value (mal-data-value hash-map))
        values)

    (hashmap (lambda (key value)
               (declare (ignorable key))
               (push value values))
             hash-map-value)

    (make-mal-list (nreverse values))))

(defmal sequential? (value)
  (wrap-boolean (or (mal-vector-p value) (mal-list-p value))))

(defmal readline (prompt)
  (format *standard-output* (mal-data-value prompt))
  (force-output *standard-output*)
  (make-mal-string (read-line *standard-input* nil)))

(defmal string? (value)
  (wrap-boolean (mal-string-p value)))

(defmal time-ms ()
  (make-mal-number (round (/ (get-internal-real-time)
                             (/ internal-time-units-per-second
                                1000)))))

(defmal conj (value &rest elements)
  (cond ((mal-list-p value)
         (make-mal-list (append (nreverse elements)
                                (mal-data-value value))))
        ((mal-vector-p value)
         (make-mal-vector (concatenate 'vector
                                       (mal-data-value value)
                                       elements)))
        (t (error 'mal-user-exception))))

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
             (types:string #'make-mal-string)
             (types:symbol #'make-mal-symbol)
             (types:list #'make-mal-list)
             (types:vector #'make-mal-vector)
             (types:hash-map #'make-mal-hash-map)
             (types:fn #'make-mal-fn)
             (types:builtin-fn #'make-mal-builtin-fn))
           (mal-data-value value)
           :meta meta
           :attrs (mal-data-attrs value)))

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
  (wrap-value (eval (read-from-string (mal-data-value code)))
              (and booleanp (mal-data-value booleanp))
              (and listp (mal-data-value listp))))
