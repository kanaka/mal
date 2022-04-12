(require 'mal/types)
(require 'mal/env)
(require 'mal/reader)
(require 'mal/printer)

(defvar repl-env (mal-env))
(mal-env-set repl-env '+ (lambda (a b) (mal-number (+ (mal-value a) (mal-value b)))))
(mal-env-set repl-env '- (lambda (a b) (mal-number (- (mal-value a) (mal-value b)))))
(mal-env-set repl-env '* (lambda (a b) (mal-number (* (mal-value a) (mal-value b)))))
(mal-env-set repl-env '/ (lambda (a b) (mal-number (/ (mal-value a) (mal-value b)))))

(defun READ (input)
  (read-str input))

(defun EVAL (ast env)
  (let ((dbgeval (mal-env-get env 'DEBUG-EVAL)))
    (if (and dbgeval
             (not (member (mal-type dbgeval) '(false nil))))
      (println "EVAL: %s\n" (PRINT ast))))

  (cl-case (mal-type ast)
     (list
      (let* ((a (mal-value ast))
             (a1 (cadr a))
             (a2 (nth 2 a)))
       (if a
        (cl-case (mal-value (car a))
         (def!
          (let ((identifier (mal-value a1))
                (value (EVAL a2 env)))
            (mal-env-set env identifier value)))
         (let*
          (let* ((env* (mal-env env))
                 (a1* (mal-value a1))
                 (bindings (if (vectorp a1*) (append a1* nil) a1*))
                 (form a2))
            (while bindings
              (let ((key (mal-value (pop bindings)))
                    (value (EVAL (pop bindings) env*)))
                (mal-env-set env* key value)))
            (EVAL form env*)))
         (t
          ;; not a special form
          (let ((fn (EVAL (car a) env))
                (args (mapcar (lambda (x) (EVAL x env)) (cdr a))))
            (apply fn args))))
        ast)))
     (symbol
      (let ((key (mal-value ast)))
        (or (mal-env-get env key)
            (error "'%s' not found" key))))
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
               (println (error-message-string err))))
          (setq eof t)
          ;; print final newline
          (terpri))))))

(main)
