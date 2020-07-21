(require 'mal/types)
(require 'mal/reader)
(require 'mal/printer)

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
               (princ (format "Expected '%c', got EOF\n"
                              (cl-case (cadr err)
                                (string ?\")
                                (list   ?\))
                                (vector ?\])
                                (map    ?})))))
              (error ; catch-all
               (println (error-message-string err))
               (backtrace)))
          (setq eof t)
          ;; print final newline
          (terpri))))))

(main)
