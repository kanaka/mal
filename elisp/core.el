(defun mal-seq-p (mal-object)
  (let ((type (mal-type mal-object)))
    (if (or (eq type 'list) (eq type 'vector))
        mal-true
      mal-false)))

(defun mal-listify (mal-object)
  (let ((type (mal-type mal-object)))
    (if (eq type 'vector)
        (append (mal-value mal-object) nil)
      (mal-value mal-object))))

(defun mal-= (a b)
  (let ((a-type (mal-type a))
        (b-type (mal-type b)))
    (cond
     ((and (and (not (eq a-type 'map))
                (not (eq a-type 'list))
                (not (eq a-type 'vector)))
           (and (not (eq b-type 'map))
                (not (eq b-type 'list))
                (not (eq b-type 'vector))))
      (mal-atom-= a b))
     ((and (or (eq a-type 'list) (eq a-type 'vector))
           (or (eq b-type 'list) (eq b-type 'vector)))
      (mal-seq-= a b))
     ((and (eq a-type 'map) (eq b-type 'map))
      (mal-map-= a b))
     (t
      ;; incompatible types
      nil))))

(defun mal-atom-= (a b)
  (equal (mal-value a) (mal-value b)))

(defun mal-seq-= (a b)
  (when (= (length (mal-value a))
           (length (mal-value b)))
    (when (everyp 'mal-= (mal-listify a) (mal-listify b))
      t)))

(defun everyp (predicate list-a list-b)
  (let ((everyp t))
    (while (and everyp list-a list-b)
      (let ((item-a (pop list-a))
            (item-b (pop list-b)))
        (when (not (funcall predicate item-a item-b))
          (setq everyp nil))))
    everyp))

(defun mal-map-= (a b)
  (catch 'return
    (let ((a* (mal-value a))
          (b* (mal-value b)))
      (when (= (hash-table-count a*)
               (hash-table-count b*))
        (maphash (lambda (key a-value)
                   (let ((b-value (gethash key b*)))
                     (if b-value
                         (when (not (mal-= a-value b-value))
                           (throw 'return nil))
                       (throw 'return nil))))
                 a*)
        ;; if we made it this far, the maps are equal
        t))))

(define-hash-table-test 'mal-= 'mal-= 'sxhash)

(defun mal-conj (seq &rest args)
  (let ((type (mal-type seq))
        (value (mal-value seq)))
    (if (eq type 'vector)
        (mal-vector (vconcat (append (append value nil) args)))
      (while args
        (push (pop args) value))
      (mal-list value))))

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
                          (let* ((list (mal-listify seq))
                                 (value (car list)))
                            (or value mal-nil))))))
    (rest . ,(mal-fn (lambda (seq) (mal-list (cdr (mal-listify seq))))))

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

    (symbol? . ,(mal-fn (lambda (arg) (if (mal-symbol-p arg) mal-true mal-false))))
    (keyword? . ,(mal-fn (lambda (arg) (if (mal-keyword-p arg) mal-true mal-false))))
    (string? . ,(mal-fn (lambda (arg) (if (mal-string-p arg) mal-true mal-false))))
    (vector? . ,(mal-fn (lambda (arg) (if (mal-vector-p arg) mal-true mal-false))))
    (map? . ,(mal-fn (lambda (arg) (if (mal-map-p arg) mal-true mal-false))))

    (symbol . ,(mal-fn (lambda (string) (mal-symbol (intern (mal-value string))))))
    (keyword . ,(mal-fn (lambda (string) (mal-keyword (intern (concat ":" (mal-value string)))))))
    (vector . ,(mal-fn (lambda (&rest args) (mal-vector (vconcat args)))))
    (hash-map . ,(mal-fn (lambda (&rest args)
                           (let ((map (make-hash-table :test 'mal-=)))
                             (while args
                               (puthash (pop args) (pop args) map))
                             (mal-map map)))))

    (sequential? . ,(mal-fn 'mal-seq-p))

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
                            ;; TODO: doesn't work on hashtables
                            (let ((mal-object* (copy-tree mal-object t)))
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
    ))
