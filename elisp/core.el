(defun mal-seq-p (mal-object)
  (let ((type (mal-type mal-object)))
    (if (or (eq type 'list) (eq type 'vector))
        (mal-true)
      (mal-false))))

(defun mal-listify (mal-object)
  (let ((type (mal-type mal-object)))
    (if (eq type 'vector)
        (mal-list (append (mal-value mal-object) nil))
      mal-object)))

(defun everyp (predicate list-a list-b)
  (let ((everyp t))
    (while (and everyp list-a list-b)
      (let ((item-a (pop list-a))
            (item-b (pop list-b)))
        (when (not (funcall predicate item-a item-b))
          (setq everyp nil))))
    everyp))

(defun mal-= (a b)
  (let ((mal-seq-a-p (mal-true-p (mal-seq-p a)))
        (mal-seq-b-p (mal-true-p (mal-seq-p b))))
    (cond
     ((and (not mal-seq-a-p) (not mal-seq-b-p))
      (equal (mal-value a) (mal-value b)))
     ((or (and (not mal-seq-a-p) mal-seq-b-p)
          (and mal-seq-a-p (not mal-seq-b-p)))
      nil)
     ((and mal-seq-a-p mal-seq-b-p
           (= (length (mal-value a))
              (length (mal-value b))))
      (if (everyp 'mal-= (mal-value (mal-listify a)) (mal-value (mal-listify b)))
          t
        nil)))))

(defvar core-ns
  `((+ . ,(mal-fn (lambda (a b) (mal-number (+ (mal-value a) (mal-value b))))))
    (- . ,(mal-fn (lambda (a b) (mal-number (- (mal-value a) (mal-value b))))))
    (* . ,(mal-fn (lambda (a b) (mal-number (* (mal-value a) (mal-value b))))))
    (/ . ,(mal-fn (lambda (a b) (mal-number (/ (mal-value a) (mal-value b))))))

    (< . ,(mal-fn (lambda (a b) (if (< (mal-value a) (mal-value b)) (mal-true) (mal-false)))))
    (<= . ,(mal-fn (lambda (a b) (if (<= (mal-value a) (mal-value b)) (mal-true) (mal-false)))))
    (> . ,(mal-fn (lambda (a b) (if (> (mal-value a) (mal-value b)) (mal-true) (mal-false)))))
    (>= . ,(mal-fn (lambda (a b) (if (>= (mal-value a) (mal-value b)) (mal-true) (mal-false)))))

    (= . ,(mal-fn (lambda (a b) (if (mal-= a b) (mal-true) (mal-false)))))

    (list . ,(mal-fn (lambda (&rest args) (mal-list args))))
    (list? . ,(mal-fn (lambda (mal-object) (if (mal-list-p mal-object) (mal-true) (mal-false)))))
    (empty? . ,(mal-fn (lambda (seq) (if (zerop (length (mal-value seq))) (mal-true) (mal-false)))))
    (count . ,(mal-fn (lambda (seq) (mal-number (if (mal-seq-p seq) (length (mal-value seq)) 0)))))

    (pr-str . ,(mal-fn (lambda (&rest args) (mal-string (mapconcat (lambda (item) (pr-str item t)) args " ")))))
    (str . ,(mal-fn (lambda (&rest args) (mal-string (mapconcat 'pr-str args "")))))
    (prn . ,(mal-fn (lambda (&rest args) (println (mapconcat (lambda (item) (pr-str item t)) args " ")) (mal-nil))))
    (println . ,(mal-fn (lambda (&rest args) (println (mapconcat 'pr-str args " ")) (mal-nil))))

    (read-string . ,(mal-fn (lambda (input) (read-str (mal-value input)))))
    (slurp . ,(mal-fn (lambda (file)
                        (with-temp-buffer
                          (insert-file-contents-literally (mal-value file))
                          (mal-string (buffer-string))))))

    (atom . ,(mal-fn (lambda (arg) (mal-atom arg))))
    (atom? . ,(mal-fn (lambda (mal-object) (if (mal-atom-p mal-object) (mal-true) (mal-false)))))
    (deref . ,(mal-fn (lambda (atom) (mal-value atom))))
    (reset! . ,(mal-fn (lambda (atom value) (setf (aref atom 1) value))))
    (swap! . ,(mal-fn (lambda (atom fn &rest args)
                        (let* ((fn* (if (mal-func-p fn) (mal-func-fn fn) fn))
                               (args* (cons (mal-value atom) args))
                               (value (apply (mal-value fn*) args*)))
                          (setf (aref atom 1) value)))))

    (cons . ,(mal-fn (lambda (arg list) (mal-list (cons arg (mal-value (mal-listify list)))))))
    (concat . ,(mal-fn (lambda (&rest lists)
                         (let ((lists* (mapcar (lambda (item) (mal-value (mal-listify item))) lists)))
                           (mal-list (apply 'append lists*))))))
    ))
