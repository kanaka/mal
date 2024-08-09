(defun mal-func (ast params env fn)
  (vector 'func (vector ast params env fn nil) nil))

(defun mal-macro (mal-func)
  (let ((v (aref mal-func 1)))
    (vector 'func
            (vector (aref v 0) (aref v 1) (aref v 2) (aref v 3) t)
            nil)))

(defun mal-func-ast (mal-func)
  (aref (aref mal-func 1) 0))

(defun mal-func-params (mal-func)
  (aref (aref mal-func 1) 1))

(defun mal-func-env (mal-func)
  (aref (aref mal-func 1) 2))

(defun mal-func-fn (mal-func)
  (aref (aref mal-func 1) 3))

(defun mal-func-macro-p (mal-func)
  (aref (aref mal-func 1) 4))

(provide 'mal/func)
