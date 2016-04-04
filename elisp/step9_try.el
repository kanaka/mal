;; -*- lexical-binding: t; -*-

(defun load-relative (file)
  (let* ((current-file (or load-file-name buffer-file-name))
         (current-file-directory (file-name-directory current-file)))
    (load (expand-file-name file current-file-directory) nil t)))

(load-relative "types.el")
(load-relative "env.el")
(load-relative "func.el")
(load-relative "reader.el")
(load-relative "printer.el")
(load-relative "core.el")

(defvar repl-env (mal-env))

(dolist (binding core-ns)
  (let ((symbol (car binding))
        (fn (cdr binding)))
    (mal-env-set repl-env symbol fn)))

(defun mal-pair-p (mal-object)
  (let ((type (mal-type mal-object))
        (value (mal-value mal-object)))
    (if (and (or (eq type 'list) (eq type 'vector))
             (not (zerop (length value))))
        t
      nil)))

(defun quasiquote (ast)
  (if (not (mal-pair-p ast))
      (mal-list (list (mal-symbol 'quote) ast))
    (let* ((a (mal-listify ast))
           (a0 (car a))
           (a0... (cdr a))
           (a1 (cadr a)))
      (cond
       ((eq (mal-value a0) 'unquote)
        a1)
       ((and (mal-pair-p a0)
             (eq (mal-value (car (mal-value a0)))
                 'splice-unquote))
        (mal-list (list (mal-symbol 'concat)
                        (cadr (mal-value a0))
                        (quasiquote (mal-list a0...)))))
       (t
        (mal-list (list (mal-symbol 'cons)
                        (quasiquote a0)
                        (quasiquote (mal-list a0...)))))))))

(defun macro-call-p (ast env)
  (when (mal-list-p ast)
    (let ((a0 (car (mal-value ast))))
      (when (mal-symbol-p a0)
        (let ((value (mal-env-find env (mal-value a0))))
          (when (and (mal-func-p value)
                     (mal-func-macro-p value))
            t))))))

(defun MACROEXPAND (ast env)
  (while (macro-call-p ast env)
    (let* ((a (mal-value ast))
           (a0* (mal-value (car a)))
           (a0... (cdr a))
           (macro (mal-env-find env a0*)))
      (setq ast (apply (mal-value (mal-func-fn macro)) a0...))))
  ast)

(defun READ (input)
  (read-str input))

(defun EVAL (ast env)
  (catch 'return
    (while t
      (when (not (mal-list-p ast))
        (throw 'return (eval-ast ast env)))

      (setq ast (MACROEXPAND ast env))
      (when (or (not (mal-list-p ast)) (not (mal-value ast)))
        (throw 'return (eval-ast ast env)))

      (let* ((a (mal-value ast))
             (a0 (car a))
             (a0* (mal-value a0))
             (a1 (cadr a))
             (a2 (nth 2 a))
             (a3 (nth 3 a)))
        (cond
         ((eq a0* 'def!)
          (let* ((identifier (mal-value a1))
                 (value (EVAL a2 env)))
            (throw 'return (mal-env-set env identifier value))))
         ((eq a0* 'let*)
          (let* ((env* (mal-env env))
                 (bindings (mal-value a1))
                 (form a2))
            (when (vectorp bindings)
              (setq bindings (append bindings nil)))
            (while bindings
              (let ((key (mal-value (pop bindings)))
                    (value (EVAL (pop bindings) env*)))
                (mal-env-set env* key value)))
            (setq env env*
                  ast form))) ; TCO
         ((eq a0* 'quote)
          (throw 'return a1))
         ((eq a0* 'quasiquote)
          (setq ast (quasiquote a1))) ; TCO
         ((eq a0* 'defmacro!)
          (let ((identifier (mal-value a1))
                (value (EVAL a2 env)))
            (setf (aref (aref value 1) 4) t)
            (throw 'return (mal-env-set env identifier value))))
         ((eq a0* 'macroexpand)
          (throw 'return (MACROEXPAND a1 env)))
         ((eq a0* 'try*)
          (condition-case err
              (throw 'return (EVAL a1 env))
            (error
             (if (and a2 (eq (mal-value (car (mal-value a2))) 'catch*))
                 (let* ((a2* (mal-value a2))
                        (identifier (mal-value (cadr a2*)))
                        (form (nth 2 a2*))
                        (err* (if (eq (car err) 'mal-custom)
                                  ;; throw
                                  (cadr err)
                                ;; normal error
                                (mal-string (error-message-string err))))
                        (env* (mal-env env (list identifier) (list err*))))
                   (throw 'return (EVAL form env*)))
               (apply 'signal err)))))
         ((eq a0* 'do)
          (let* ((a0... (cdr a))
                 (butlast (butlast a0...))
                 (last (car (last a0...))))
            (when butlast
              (eval-ast (mal-list butlast) env))
            (setq ast last))) ; TCO
         ((eq a0* 'if)
          (let* ((condition (EVAL a1 env))
                 (condition-type (mal-type condition))
                 (then a2)
                 (else a3))
            (if (and (not (eq condition-type 'false))
                     (not (eq condition-type 'nil)))
                (setq ast then) ; TCO
              (if else
                  (setq ast else) ; TCO
                (throw 'return (mal-nil))))))
         ((eq a0* 'fn*)
          (let* ((binds (mapcar 'mal-value (mal-value a1)))
                 (body a2)
                 (fn (mal-fn
                      (lambda (&rest args)
                        (let ((env* (mal-env env binds args)))
                          (EVAL body env*))))))
            (throw 'return (mal-func body binds env fn))))
         (t
          ;; not a special form
          (let* ((ast* (mal-value (eval-ast ast env)))
                 (fn (car ast*))
                 (args (cdr ast*)))
            (if (mal-func-p fn)
                (let ((env* (mal-env (mal-func-env fn)
                                     (mal-func-params fn)
                                     args)))
                  (setq env env*
                        ast (mal-func-ast fn))) ; TCO
              ;; built-in function
              (let ((fn* (mal-value fn)))
                (throw 'return (apply fn* args)))))))))))

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

(mal-env-set repl-env 'eval (mal-fn (let ((env repl-env)) (lambda (form) (EVAL form env)))))
(mal-env-set repl-env '*ARGV* (mal-list (mapcar 'mal-string (cdr argv))))

(defun PRINT (input)
  (pr-str input t))

(defun rep (input)
  (PRINT (EVAL (READ input) repl-env)))

(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")

(rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
(rep "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))")

(defun readln (prompt)
  ;; C-d throws an error
  (ignore-errors (read-from-minibuffer prompt)))

(defun println (format-string &rest args)
  (if (not args)
      (princ format-string)
    (princ (apply 'format format-string args)))
  (terpri))

(defmacro with-error-handling (&rest body)
  `(condition-case err
       (progn ,@body)
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
      (println (error-message-string err)))))

(defun main ()
  (if argv
      (with-error-handling
       (rep (format "(load-file \"%s\")" (car argv))))
    (let (eof)
      (while (not eof)
        (let ((input (readln "user> ")))
          (if input
              (with-error-handling
               (println (rep input)))
            (setq eof t)
            ;; print final newline
            (terpri)))))))

(main)
