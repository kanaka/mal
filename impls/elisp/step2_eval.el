(require 'mal/types)
(require 'mal/reader)
(require 'mal/printer)

(defvar repl-env (make-hash-table :test 'eq))
(puthash '+ (lambda (a b) (mal-number (+ (mal-value a) (mal-value b)))) repl-env)
(puthash '- (lambda (a b) (mal-number (- (mal-value a) (mal-value b)))) repl-env)
(puthash '* (lambda (a b) (mal-number (* (mal-value a) (mal-value b)))) repl-env)
(puthash '/ (lambda (a b) (mal-number (/ (mal-value a) (mal-value b)))) repl-env)

(defun READ (input)
  (read-str input))

(defun EVAL (ast env)
  ;; (println "EVAL: %s\n" (PRINT ast))
  (cl-case (mal-type ast)
     (list
      (let ((a (mal-value ast)))
      (if a
       (let* ((fn (EVAL (car a) env))
               (args (mapcar (lambda (x) (EVAL x env)) (cdr a))))
        (apply fn args))
       ast)))
     (symbol
      (let ((definition (gethash (mal-value ast) env)))
        (or definition (error "Definition not found"))))
     (vector
      (mal-vector (vconcat (mapcar (lambda (item) (EVAL item env))
                                   (mal-value ast)))))
     (map
      (let ((map (copy-hash-table (mal-value ast))))
        (maphash (lambda (key val)
                   (puthash key (EVAL val env) map))
                 map)
        (mal-map map)))
     (t
      ;; return as is
      ast)))

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
