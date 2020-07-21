(require 'cl-lib)

(defun mal-seq-p (mal-object)
  (memq (mal-type mal-object) '(list vector)))

(defun mal-listify (mal-object)
  (cl-ecase (mal-type mal-object)
    (list   (mal-value mal-object))
    (vector (append (mal-value mal-object) nil))))

(defun mal-= (a b)
  (cl-case (mal-type a)
    ((list vector) (and (mal-seq-p b)
                        (mal-seq-= (mal-listify a) (mal-listify b))))
    (map           (and (mal-map-p b)
                        (mal-map-= (mal-value a) (mal-value b))))
    (t             (equal (mal-value a) (mal-value b)))))

(defun mal-seq-= (a b)
  (if a
      (and b
           (mal-= (car a) (car b))
           (mal-seq-= (cdr a) (cdr b)))
    (null b)))

(defun mal-map-= (a b)
  (when (= (hash-table-count a)
           (hash-table-count b))
    (catch 'return
      (maphash (lambda (key a-value)
                 (let ((b-value (gethash key b)))
                   (unless (and b-value
                                (mal-= a-value b-value))
                     (throw 'return nil))))
               a)
      ;; if we made it this far, the maps are equal
      t)))

(define-hash-table-test 'mal-= 'mal-= 'sxhash)

(defun mal-conj (seq &rest args)
  (let ((value (mal-value seq)))
    (cl-ecase (mal-type seq)
     (vector
        (mal-vector (vconcat (append (append value nil) args))))
     (list
      (while args
        (push (pop args) value))
      (mal-list value)))))

(defun elisp-to-mal (arg)
  (cond
   ((not arg)
    mal-nil)
   ((eq arg t)
    mal-true)
   ((numberp arg)
    (mal-number arg))
   ((stringp arg)
    (mal-string arg))
   ((keywordp arg)
    (mal-keyword arg))
   ((symbolp arg)
    (mal-symbol arg))
   ((consp arg)
    (mal-list (mapcar 'elisp-to-mal arg)))
   ((vectorp arg)
    (mal-vector (vconcat (mapcar 'elisp-to-mal arg))))
   ((hash-table-p arg)
    (let ((output (make-hash-table :test 'mal-=)))
      (maphash
       (lambda (key value)
         (puthash (elisp-to-mal key) (elisp-to-mal value) output))
       arg)
      (mal-map output)))
   (t
    ;; represent anything else as printed arg
    (mal-string (format "%S" arg)))))

(defvar core-ns
  `((+ . ,(mal-fn (lambda (a b) (mal-number (+ (mal-value a) (mal-value b))))))
    (- . ,(mal-fn (lambda (a b) (mal-number (- (mal-value a) (mal-value b))))))
    (* . ,(mal-fn (lambda (a b) (mal-number (* (mal-value a) (mal-value b))))))
    (/ . ,(mal-fn (lambda (a b) (mal-number (/ (mal-value a) (mal-value b))))))

    (< . ,(mal-fn (lambda (a b) (if (< (mal-value a) (mal-value b)) mal-true mal-false))))
    (<= . ,(mal-fn (lambda (a b) (if (<= (mal-value a) (mal-value b)) mal-true mal-false))))
    (> . ,(mal-fn (lambda (a b) (if (> (mal-value a) (mal-value b)) mal-true mal-false))))
    (>= . ,(mal-fn (lambda (a b) (if (>= (mal-value a) (mal-value b)) mal-true mal-false))))

    (= . ,(mal-fn (lambda (a b) (if (mal-= a b) mal-true mal-false))))

    (list . ,(mal-fn (lambda (&rest args) (mal-list args))))
    (list? . ,(mal-fn (lambda (mal-object) (if (mal-list-p mal-object) mal-true mal-false))))
    (empty? . ,(mal-fn (lambda (seq) (if (zerop (length (mal-value seq))) mal-true mal-false))))
    (count . ,(mal-fn (lambda (seq) (mal-number (if (mal-seq-p seq) (length (mal-value seq)) 0)))))

    (pr-str . ,(mal-fn (lambda (&rest args) (mal-string (mapconcat (lambda (item) (pr-str item t)) args " ")))))
    (str . ,(mal-fn (lambda (&rest args) (mal-string (mapconcat 'pr-str args "")))))
    (prn . ,(mal-fn (lambda (&rest args) (println (mapconcat (lambda (item) (pr-str item t)) args " ")) mal-nil)))
    (println . ,(mal-fn (lambda (&rest args) (println (mapconcat 'pr-str args " ")) mal-nil)))

    (read-string . ,(mal-fn (lambda (input) (read-str (mal-value input)))))
    (slurp . ,(mal-fn (lambda (file)
                        (with-temp-buffer
                          (insert-file-contents-literally (mal-value file))
                          (mal-string (buffer-string))))))

    (atom . ,(mal-fn (lambda (arg) (mal-atom arg))))
    (atom? . ,(mal-fn (lambda (mal-object) (if (mal-atom-p mal-object) mal-true mal-false))))
    (deref . ,(mal-fn (lambda (atom) (mal-value atom))))
    (reset! . ,(mal-fn (lambda (atom value) (setf (aref atom 1) value))))
    (swap! . ,(mal-fn (lambda (atom fn &rest args)
                        (let* ((fn* (if (mal-func-p fn) (mal-func-fn fn) fn))
                               (args* (cons (mal-value atom) args))
                               (value (apply (mal-value fn*) args*)))
                          (setf (aref atom 1) value)))))

    (vec . ,(mal-fn (lambda (seq) (if (mal-vector-p seq) seq (mal-vector (mal-value seq))))))
    (cons . ,(mal-fn (lambda (arg list) (mal-list (cons arg (mal-listify list))))))
    (concat . ,(mal-fn (lambda (&rest lists)
                         (let ((lists* (mapcar (lambda (item) (mal-listify item)) lists)))
                           (mal-list (apply 'append lists*))))))

    (nth . ,(mal-fn (lambda (seq index)
                      (let ((i (mal-value index))
                            (list (mal-listify seq)))
                        (or (nth i list)
                            (error "Args out of range: %s, %d" (pr-str seq) i))))))
    (first . ,(mal-fn (lambda (seq)
                        (if (mal-nil-p seq)
                            mal-nil
                          (or (car (mal-listify seq)) mal-nil)))))
    (rest . ,(mal-fn (lambda (seq) (mal-list (unless (mal-nil-p seq) (cdr (mal-listify seq)))))))

    (throw . ,(mal-fn (lambda (mal-object) (signal 'mal-custom (list mal-object)))))

    (apply . ,(mal-fn (lambda (fn &rest args)
                        (let* ((butlast (butlast args))
                               (last (mal-listify (car (last args))))
                               (fn* (if (mal-func-p fn) (mal-func-fn fn) fn))
                               (args* (append butlast last)))
                          (apply (mal-value fn*) args*)))))
    (map . ,(mal-fn (lambda (fn seq)
                      (let ((fn* (if (mal-func-p fn) (mal-func-fn fn) fn)))
                        (mal-list (mapcar (mal-value fn*) (mal-value seq)))))))

    (nil? . ,(mal-fn (lambda (arg) (if (mal-nil-p arg) mal-true mal-false))))
    (true? . ,(mal-fn (lambda (arg) (if (mal-true-p arg) mal-true mal-false))))
    (false? . ,(mal-fn (lambda (arg) (if (mal-false-p arg) mal-true mal-false))))

    (number? . ,(mal-fn (lambda (arg) (if (mal-number-p arg) mal-true mal-false))))
    (symbol? . ,(mal-fn (lambda (arg) (if (mal-symbol-p arg) mal-true mal-false))))
    (keyword? . ,(mal-fn (lambda (arg) (if (mal-keyword-p arg) mal-true mal-false))))
    (string? . ,(mal-fn (lambda (arg) (if (mal-string-p arg) mal-true mal-false))))
    (vector? . ,(mal-fn (lambda (arg) (if (mal-vector-p arg) mal-true mal-false))))
    (map? . ,(mal-fn (lambda (arg) (if (mal-map-p arg) mal-true mal-false))))

    (symbol . ,(mal-fn (lambda (string) (mal-symbol (intern (mal-value string))))))
    (keyword . ,(mal-fn (lambda (x) (if (mal-keyword-p x) x (mal-keyword (intern (concat ":" (mal-value x))))))))
    (vector . ,(mal-fn (lambda (&rest args) (mal-vector (vconcat args)))))
    (hash-map . ,(mal-fn (lambda (&rest args)
                           (let ((map (make-hash-table :test 'mal-=)))
                             (while args
                               (puthash (pop args) (pop args) map))
                             (mal-map map)))))

    (sequential? . ,(mal-fn (lambda (mal-object) (if (mal-seq-p mal-object) mal-true mal-false))))
    (fn? . ,(mal-fn (lambda (arg) (if (or (mal-fn-p arg)
                                          (and (mal-func-p arg)
                                               (not (mal-func-macro-p arg))))
                                      mal-true
                                    mal-false))))
    (macro? . ,(mal-fn (lambda (arg) (if (and (mal-func-p arg)
                                              (mal-func-macro-p arg))
                                         mal-true
                                       mal-false))))

    (get . ,(mal-fn (lambda (map key) (if (mal-map-p map) (or (gethash key (mal-value map)) mal-nil) mal-nil))))
    (contains? . ,(mal-fn (lambda (map key) (if (gethash key (mal-value map)) mal-true mal-false))))
    (assoc . ,(mal-fn (lambda (map &rest args)
                        (let ((map* (copy-hash-table (mal-value map))))
                          (while args
                            (puthash (pop args) (pop args) map*))
                          (mal-map map*)))))
    (dissoc . ,(mal-fn (lambda (map &rest args)
                         (let ((map* (copy-hash-table (mal-value map))))
                           (while args
                             (remhash (pop args) map*))
                           (mal-map map*)))))
    (keys . ,(mal-fn (lambda (map) (let (keys)
                                     (maphash (lambda (key value) (push key keys))
                                              (mal-value map))
                                     (mal-list keys)))))
    (vals . ,(mal-fn (lambda (map) (let (vals)
                                     (maphash (lambda (key value) (push value vals))
                                              (mal-value map))
                                     (mal-list vals)))))

    (readline . ,(mal-fn (lambda (prompt)
                           (let ((ret (readln (mal-value prompt))))
                             (if ret
                                 (mal-string ret)
                               mal-nil)))))

    (meta . ,(mal-fn (lambda (mal-object) (or (mal-meta mal-object) mal-nil))))
    (with-meta . ,(mal-fn (lambda (mal-object meta)
                            (let ((mal-object* (copy-sequence mal-object)))
                              (setf (aref mal-object* 2) meta)
                              mal-object*))))

    (time-ms . ,(mal-fn (lambda () (mal-number (floor (* (float-time) 1000))))))

    (conj . ,(mal-fn 'mal-conj))
    (seq . ,(mal-fn (lambda (mal-object)
                      (let ((type (mal-type mal-object))
                            (value (mal-value mal-object)))
                        (cond
                         ((or (eq type 'list) (eq type 'vector))
                          (if (and value (not (zerop (length value))))
                              (mal-list (mal-listify mal-object))
                            mal-nil))
                         ((eq type 'string)
                          (if (not (zerop (length value)))
                              (mal-list (mapcar (lambda (item) (mal-string (char-to-string item)))
                                                (append value nil)))
                            mal-nil))
                         (t
                          mal-nil))))))

    (elisp-eval . ,(mal-fn (lambda (string) (elisp-to-mal (eval (read (mal-value string)))))))
    ))

(provide 'mal/core)
