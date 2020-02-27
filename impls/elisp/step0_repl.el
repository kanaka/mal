(defun READ (input)
  input)

(defun EVAL (input)
  input)

(defun PRINT (input)
  input)

(defun readln (prompt)
  ;; C-d throws an error
  (ignore-errors (read-from-minibuffer prompt)))

(defun println (format-string &rest args)
  (if (not args)
      (princ format-string)
    (princ (apply 'format format-string args)))
  (terpri))

(defun main ()
  (let (eof)
    (while (not eof)
      (let ((input (readln "user> ")))
        (if input
            (println input)
          (setq eof t)
          ;; print final newline
          (terpri))))))

(main)
