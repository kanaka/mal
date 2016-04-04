(defun load-relative (file)
  (let* ((current-file (or load-file-name buffer-file-name))
         (current-file-directory (file-name-directory current-file)))
    (load (expand-file-name file current-file-directory) nil t)))

(load-relative "types.el")
(load-relative "reader.el")
(load-relative "printer.el")

(defvar repl-env (make-hash-table :test 'eq))
(puthash '+ (lambda (a b) (mal-number (+ (mal-value a) (mal-value b)))) repl-env)
(puthash '- (lambda (a b) (mal-number (- (mal-value a) (mal-value b)))) repl-env)
(puthash '* (lambda (a b) (mal-number (* (mal-value a) (mal-value b)))) repl-env)
(puthash '/ (lambda (a b) (mal-number (/ (mal-value a) (mal-value b)))) repl-env)

(defun READ (input)
  (read-str input))

(defun EVAL (ast env)
  (if (and (mal-list-p ast) (mal-value ast))
      (let* ((ast* (mal-value (eval-ast ast env)))
             (fn (car ast*))
             (args (cdr ast*)))
        (apply fn args))
    (eval-ast ast env)))

(defun eval-ast (ast env)
  (let ((type (mal-type ast))
        (value (mal-value ast)))
    (cond
     ((eq type 'symbol)
      (let ((definition (gethash value env)))
        (or definition (error "Definition not found"))))
     ((eq type 'list)
      (mal-list (mapcar (lambda (item) (EVAL item env)) value)))
     ((eq type 'vector)
      (mal-vector (vconcat (mapcar (lambda (item) (EVAL item env)) value))))
     ((eq type 'map)
      (let ((map (copy-hash-table value)))
        (maphash (lambda (key value)
                   (puthash key (EVAL value env) map))
                 map)
        (mal-map map)))
     (t
      ;; return as is
      ast))))

(defun PRINT (input)
  (pr-str input t))

(defun rep (input)
  (PRINT (EVAL (READ input) repl-env)))

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
