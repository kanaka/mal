(defun mal-func (ast params env fn &optional macrop)
  (vector 'func (vector ast params env fn macrop)))

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
