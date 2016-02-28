(defun load-relative (file)
  (let* ((current-file (or load-file-name buffer-file-name))
         (current-file-directory (file-name-directory current-file)))
    (load (expand-file-name file current-file-directory) nil t)))

(load-relative "types.el")
(load-relative "reader.el")
(load-relative "printer.el")

(defun READ (input)
  (read-str input))

(defun EVAL (input)
  input)

(defun PRINT (input)
  (pr-str input t))

(defun rep (input)
  (PRINT (EVAL (READ input))))

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
            (condition-case err
                (println (rep input))
              (end-of-token-stream
               ;; empty input, carry on
               )
              (unterminated-sequence
               (let* ((type (cadr err))
                      (end
                       (cond
                        ((eq type 'string) ?\")
                        ((eq type 'list) ?\))
                        ((eq type 'vector) ?\])
                        ((eq type 'map) ?}))))
                 (princ (format "Expected '%c', got EOF\n" end))))
              (error ; catch-all
               (println (error-message-string err))
               (backtrace)))
          (setq eof t)
          ;; print final newline
          (terpri))))))

(main)
