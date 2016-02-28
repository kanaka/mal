(defun mal-env (&optional outer)
  (vector 'env (vector (make-hash-table :test 'eq) outer)))

(defun mal-env-set (env key value)
  (let ((data (aref (aref env 1) 0)))
    (puthash key value data)))

(defun mal-env-find (env key)
  (let* ((data (aref (aref env 1) 0))
         (value (gethash key data)))
    (if (not value)
        (let ((outer (aref (aref env 1) 1)))
          (when outer
            (mal-env-find outer key)))
      value)))

(defun mal-env-get (env key)
  (let ((value (mal-env-find env key)))
    (if (not value)
        (error "%s not found" key)
      value)))
