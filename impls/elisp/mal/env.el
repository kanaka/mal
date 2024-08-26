(require 'mal/types)

;; An env is represented by an elisp list of hash-tables. In other words
;;  * car: a hash-table
;;  * cdr: the outer environment or ()
;; Keys are elisp symbols.

(defun mal-env (&optional outer binds exprs)
  (let ((env (cons (make-hash-table :test 'eq) outer))
        key)
    (while (setq key (pop binds))
      (if (eq key '&)
          (mal-env-set env (pop binds) (mal-list exprs))
        (mal-env-set env key (pop exprs))))
    env))

(defun mal-env-set (env key value)
  (let ((data (car env)))
    (puthash key value data)))

(defun mal-env-get (env key)
  (let (value)
    (while (and (not (setq value (gethash key (pop env))))
                env))
    value))

(provide 'mal/env)
