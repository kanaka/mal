(require 'seq)
(require 'mal/types)

(defun mal-boolean (value) (if value mal-true mal-false))

(defun mal-= (a b)
  (let (va vb)
    (cond
     ((or (setq va (mal-seq-value a)) (mal-list-p a))
      (and (or (setq vb (mal-seq-value b)) (mal-list-p b))
           (mal-seq-= va vb)))
     ((setq va (mal-number-value a))              (equal va (mal-number-value b)))
     ((setq va (mal-string-value a))              (equal va (mal-string-value b)))
     ((setq va (mal-symbol-value a))              (eq va (mal-symbol-value b)))
     ((setq va (mal-keyword-value a))             (equal va (mal-keyword-value b)))
     ((setq va (mal-map-value a))                 (and (setq vb (mal-map-value b))
                                                       (mal-map-= va vb)))
     (t                                           (eq a b)))))

(defun mal-seq-= (a b)
  (let* ((len (seq-length a))
         (res (= len (seq-length b))))
    (while (and res (< 0 len))
      (setq len (1- len))
      (unless (mal-= (seq-elt a len) (seq-elt b len))
        (setq res nil)))
    res))

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
  (let (value)
    (cond
     ((setq value (mal-vector-value seq))
      (mal-vector (vconcat value args)))
     ((setq value (mal-list-value seq))
      (mal-list (append (reverse args) value)))
     ((mal-list-p seq)
      (mal-list (reverse args)))
     (t (error "seq: bad type")))))

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
    (mal-keyword (symbol-name arg)))
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

(defconst core-ns
  '((+ . (lambda (a b) (mal-number (+ (mal-number-value a) (mal-number-value b)))))
    (- . (lambda (a b) (mal-number (- (mal-number-value a) (mal-number-value b)))))
    (* . (lambda (a b) (mal-number (* (mal-number-value a) (mal-number-value b)))))
    (/ . (lambda (a b) (mal-number (/ (mal-number-value a) (mal-number-value b)))))

    (< . (lambda (a b) (mal-boolean (< (mal-number-value a) (mal-number-value b)))))
    (<= . (lambda (a b) (mal-boolean (<= (mal-number-value a) (mal-number-value b)))))
    (> . (lambda (a b) (mal-boolean (> (mal-number-value a) (mal-number-value b)))))
    (>= . (lambda (a b) (mal-boolean (>= (mal-number-value a) (mal-number-value b)))))

    (= . (lambda (a b) (mal-boolean (mal-= a b))))

    (list . (lambda (&rest args) (mal-list args)))
    (list? . (lambda (mal-object) (mal-boolean (mal-list-p mal-object))))
    (empty? . (lambda (seq) (mal-boolean (seq-empty-p (mal-seq-value seq)))))
    (count . (lambda (seq) (mal-number (length (mal-seq-value seq)))))

    (pr-str . (lambda (&rest args) (mal-string (pr-join args t " "))))
    (str . (lambda (&rest args) (mal-string (pr-join args nil ""))))
    (prn . (lambda (&rest args)
             (println (pr-join args t " "))
             mal-nil))
    (println . (lambda (&rest args)
                 (println (pr-join args nil " "))
                 mal-nil))

    (read-string . (lambda (input) (read-str (mal-string-value input))))
    (slurp . (lambda (file)
                        (with-temp-buffer
                          (insert-file-contents-literally (mal-string-value file))
                          (mal-string (buffer-string)))))

    (atom . mal-atom)
    (atom? . (lambda (mal-object) (mal-boolean (mal-atom-value mal-object))))
    (deref . mal-atom-value)
    (reset! . (lambda (atom value)
                (mal-reset atom value)
                value))
    (swap! . (lambda (atom fn &rest args)
               (let ((value (apply (or (mal-func-value fn)
                                       (mal-fn-core-value fn))
                                   (mal-atom-value atom)
                                   args)))
                 (mal-reset atom value)
                 value)))

    (vec . (lambda (seq)
             (if (mal-vector-value seq)
                 seq
               (mal-vector (seq-into (mal-list-value seq) 'vector)))))
    (cons . (lambda (arg seq)
              (let ((value (mal-vector-value seq)))
                (mal-list (cons arg (if value
                                        (seq-into value 'list)
                                      (mal-list-value seq)))))))
    (concat . (lambda (&rest lists)
                (mal-list (seq-mapcat 'mal-seq-value lists 'list))))

    (nth . (lambda (seq index)
             (let ((list (mal-seq-value seq))
                   (i (mal-number-value index)))
               ;; seq-elt returns nil for a list and a bad index
               (or (seq-elt (mal-seq-value seq) (mal-number-value index))
                   (error "Args out of range: %s, %d" (pr-str seq t) i)))))

    (first . (lambda (seq)
               (let ((value (mal-seq-value seq)))
                 (if (seq-empty-p value)
                            mal-nil
                   (seq-first value)))))
    (rest . (lambda (seq)
              (let ((value(mal-vector-value seq)))
                (mal-list (cdr (if value
                                   (seq-into value 'list)
                                 (mal-list-value seq)))))))

    (throw . (lambda (mal-object) (signal 'mal-custom (list mal-object))))

    (apply . (lambda (fn &rest args)
               (let ((butlast (butlast args))
                     (last (mal-seq-value (car (last args))))
                     (fn* (or (mal-func-value fn)
                              (mal-fn-core-value fn)
                              (mal-macro-value fn))))
                 (apply fn* (seq-concatenate 'list butlast last)))))
    (map . (lambda (fn seq)
             (mal-list (mapcar (or (mal-func-value fn) (mal-fn-core-value fn))
                               (mal-seq-value seq)))))

    (nil? . (lambda (arg) (mal-boolean (eq mal-nil arg))))
    (true? . (lambda (arg) (mal-boolean (eq mal-true arg))))
    (false? . (lambda (arg) (mal-boolean (eq mal-false arg))))

    (number? . (lambda (arg) (mal-boolean (mal-number-value arg))))
    (symbol? . (lambda (arg) (mal-boolean (mal-symbol-value arg))))
    (keyword? . (lambda (arg) (mal-boolean (mal-keyword-value arg))))
    (string? . (lambda (arg) (mal-boolean (mal-string-value arg))))
    (vector? . (lambda (arg) (mal-boolean (mal-vector-value arg))))
    (map? . (lambda (arg) (mal-boolean (mal-map-value arg))))

    (symbol . (lambda (string) (mal-symbol (intern (mal-string-value string)))))
    (keyword . (lambda (x)
                 (let ((value (mal-string-value x)))
                   (if value
                       (mal-keyword (concat ":" value))
                     x))))

    (vector . (lambda (&rest args) (mal-vector (seq-into args 'vector))))
    (hash-map . (lambda (&rest args)
                           (let ((map (make-hash-table :test 'mal-=)))
                             (while args
                               (puthash (pop args) (pop args) map))
                             (mal-map map))))

    (sequential? . (lambda (mal-object)
                     (mal-boolean (or (mal-list-p mal-object)
                                      (mal-vector-value mal-object)))))
    (fn? . (lambda (arg) (mal-boolean (or (mal-fn-core-value arg)
                                          (mal-func-value arg)))))
    (macro? . (lambda (arg) (mal-boolean (mal-macro-value arg))))

    (get . (lambda (map key)
             (or (let ((value (mal-map-value map)))
                   (when value
                     (gethash key value)))
                 mal-nil)))
    (contains? . (lambda (map key)
                   (mal-boolean (gethash key (mal-map-value map)))))
    (assoc . (lambda (map &rest args)
                        (let ((map* (copy-hash-table (mal-map-value map))))
                          (while args
                            (puthash (pop args) (pop args) map*))
                          (mal-map map*))))
    (dissoc . (lambda (map &rest args)
                         (let ((map* (copy-hash-table (mal-map-value map))))
                           (dolist (k args)
                             (remhash k map*))
                           (mal-map map*))))
    (keys . (lambda (map) (let (keys)
                                     (maphash (lambda (key _value) (push key keys))
                                              (mal-map-value map))
                                     (mal-list keys))))
    (vals . (lambda (map) (let (vals)
                                     (maphash (lambda (_key value) (push value vals))
                                              (mal-map-value map))
                                     (mal-list vals))))

    (readline . (lambda (prompt)
                  (or (mal-string (readln (mal-string-value prompt)))
                      mal-nil)))

    (meta . mal-meta)
    (with-meta . with-meta)

    (time-ms . (lambda () (mal-number (floor (* (float-time) 1000)))))

    (conj . mal-conj)
    (seq . (lambda (mal-object)
             (let (value)
               (or
                        (cond
                         ((setq value (mal-list-value mal-object))
                          mal-object)
                         ((and (setq value (mal-vector-value mal-object))
                               (not (seq-empty-p value)))
                          (mal-list (seq-into value 'list)))
                         ((and (setq value (mal-string-value mal-object))
                               (not (seq-empty-p value)))
                          (mal-list (mapcar (lambda (item) (mal-string (char-to-string item)))
                                            value))))
                        mal-nil))))

    (elisp-eval . (lambda (string)
                    (elisp-to-mal (eval (read (mal-string-value string))))))
    ))

(provide 'mal/core)
