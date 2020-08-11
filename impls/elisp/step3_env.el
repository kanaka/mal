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
  (if (and (mal-list-p ast) (mal-value ast))
      (let* ((a (mal-value ast))
             (a1 (cadr a))
             (a1* (mal-value a1))
             (a2 (nth 2 a)))
        (cl-case (mal-value (car a))
         (def!
          (let ((identifier a1*)
                (value (EVAL a2 env)))
            (mal-env-set env identifier value)))
         (let*
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
  (let ((value (mal-value ast)))
    (cl-case (mal-type ast)
     (symbol
      (let ((definition (mal-env-get env value)))
        (or definition (error "Definition not found"))))
     (list
      (mal-list (mapcar (lambda (item) (EVAL item env)) value)))
     (vector
      (mal-vector (vconcat (mapcar (lambda (item) (EVAL item env)) value))))
     (map
      (let ((map (copy-hash-table value)))
        (maphash (lambda (key val)
                   (puthash key (EVAL val env) map))
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
