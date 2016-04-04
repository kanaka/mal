(defun load-relative (file)
  (let* ((current-file (or load-file-name buffer-file-name))
         (current-file-directory (file-name-directory current-file)))
    (load (expand-file-name file current-file-directory) nil t)))

(load-relative "types.el")
(load-relative "env.el")
(load-relative "reader.el")
(load-relative "printer.el")

(defvar repl-env (mal-env))
(mal-env-set repl-env '+ (lambda (a b) (mal-number (+ (mal-value a) (mal-value b)))))
(mal-env-set repl-env '- (lambda (a b) (mal-number (- (mal-value a) (mal-value b)))))
(mal-env-set repl-env '* (lambda (a b) (mal-number (* (mal-value a) (mal-value b)))))
(mal-env-set repl-env '/ (lambda (a b) (mal-number (/ (mal-value a) (mal-value b)))))

(defun READ (input)
  (read-str input))

(defun EVAL (ast env)
  (if (and (mal-list-p ast) (mal-value ast))
      (let* ((a (mal-value ast))
             (a0 (car a))
             (a0* (mal-value a0))
             (a1 (cadr a))
             (a1* (mal-value a1))
             (a2 (nth 2 a)))
        (cond
         ((eq a0* 'def!)
          (let ((identifier a1*)
                (value (EVAL a2 env)))
            (mal-env-set env identifier value)))
         ((eq a0* 'let*)
          (let ((env* (mal-env env))
                (bindings (if (vectorp a1*) (append a1* nil) a1*))
                (form a2))
            (while bindings
              (let ((key (mal-value (pop bindings)))
                    (value (EVAL (pop bindings) env*)))
                (mal-env-set env* key value)))
            (EVAL form env*)))
         (t
          ;; not a special form
          (let* ((ast* (mal-value (eval-ast ast env)))
                 (fn (car ast*))
                 (args (cdr ast*)))
            (apply fn args)))))
    (eval-ast ast env)))

(defun eval-ast (ast env)
  (let ((type (mal-type ast))
        (value (mal-value ast)))
    (cond
     ((eq type 'symbol)
      (let ((definition (mal-env-get env value)))
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
               (println (error-message-string err))))
          (setq eof t)
          ;; print final newline
          (terpri))))))

(main)
