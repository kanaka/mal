(defun READ (input)
  input)

(defun EVAL (input)
  input)

(defun PRINT (input)
  input)

(defun rep (input)
  (PRINT (EVAL (READ input))))

(defun readln (prompt)
  ;; C-d throws an error
  (ignore-errors (read-from-minibuffer prompt)))

(defun println (format-string &rest args)
  (princ (if args
             (apply 'format format-string args)
           format-string))
  (terpri))

(defun main ()
  (let (input)
    (while (setq input (readln "user> "))
      (println (rep input)))
    ;; print final newline
    (terpri)))

(main)
