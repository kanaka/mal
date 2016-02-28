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
  '((+ . (lambda (a b) (mal-number (+ (mal-value a) (mal-value b)))))
    (- . (lambda (a b) (mal-number (- (mal-value a) (mal-value b)))))
    (* . (lambda (a b) (mal-number (* (mal-value a) (mal-value b)))))
    (/ . (lambda (a b) (mal-number (/ (mal-value a) (mal-value b)))))

    (< . (lambda (a b) (if (< (mal-value a) (mal-value b)) (mal-true) (mal-false))))
    (<= . (lambda (a b) (if (<= (mal-value a) (mal-value b)) (mal-true) (mal-false))))
    (> . (lambda (a b) (if (> (mal-value a) (mal-value b)) (mal-true) (mal-false))))
    (>= . (lambda (a b) (if (>= (mal-value a) (mal-value b)) (mal-true) (mal-false))))

    (= . (lambda (a b) (if (mal-= a b) (mal-true) (mal-false))))

    (list . (lambda (&rest args) (mal-list args)))
    (list? . (lambda (mal-object) (if (mal-list-p mal-object) (mal-true) (mal-false))))
    (empty? . (lambda (seq) (if (zerop (length (mal-value seq))) (mal-true) (mal-false))))
    (count . (lambda (seq) (mal-number (if (mal-seq-p seq) (length (mal-value seq)) 0))))

    (pr-str . (lambda (&rest args) (mal-string (mapconcat (lambda (item) (pr-str item t)) args " "))))
    (str . (lambda (&rest args) (mal-string (mapconcat 'pr-str args ""))))
    (prn . (lambda (&rest args) (println (mapconcat (lambda (item) (pr-str item t)) args " ")) (mal-nil)))
    (println . (lambda (&rest args) (println (mapconcat 'pr-str args " ")) (mal-nil)))
    ))
