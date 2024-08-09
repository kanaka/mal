;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'mal/types)
(require 'mal/func)
(require 'mal/env)
(require 'mal/reader)
(require 'mal/printer)
(require 'mal/core)

(defvar repl-env (mal-env))

(dolist (binding core-ns)
  (let ((symbol (car binding))
        (fn (cdr binding)))
    (mal-env-set repl-env symbol fn)))

(defun starts-with-p (ast sym)
  (let ((l (mal-value ast)))
    (and l
         (let ((s (car l)))
           (and (mal-symbol-p s)
                (eq (mal-value s) sym))))))

(defun qq-reducer (elt acc)
  (mal-list (if (and (mal-list-p elt)
                     (starts-with-p elt 'splice-unquote))
                (list (mal-symbol 'concat) (cadr (mal-value elt)) acc)
              (list (mal-symbol 'cons) (quasiquote elt) acc))))

(defun qq-iter (elts)
  (cl-reduce 'qq-reducer elts :from-end t :initial-value (mal-list nil)))

(defun quasiquote (ast)
  (cl-case (mal-type ast)
    (list         (if (starts-with-p ast 'unquote)
                      (cadr (mal-value ast))
                    (qq-iter (mal-value ast))))
    (vector       (mal-list (list (mal-symbol 'vec) (qq-iter (mal-value ast)))))
    ((map symbol) (mal-list (list (mal-symbol 'quote) ast)))
    (t            ast)))

(defun READ (input)
  (read-str input))

(defun EVAL (ast env)
  (catch 'return
    (while t

     (let ((dbgeval (mal-env-get env 'DEBUG-EVAL)))
       (if (and dbgeval
                (not (member (mal-type dbgeval) '(false nil))))
         (println "EVAL: %s\n" (PRINT ast))))

     (cl-case (mal-type ast)

     (list
      (let* ((a (mal-value ast))
             (a1 (cadr a))
             (a2 (nth 2 a))
             (a3 (nth 3 a)))
        (unless a (throw 'return ast))
        (cl-case (mal-value (car a))
         (def!
          (let ((identifier (mal-value a1))
                (value (EVAL a2 env)))
            (throw 'return (mal-env-set env identifier value))))
         (let*
          (let ((env* (mal-env env))
                (bindings (mal-listify a1))
                (form a2))
            (while bindings
              (let ((key (mal-value (pop bindings)))
                    (value (EVAL (pop bindings) env*)))
                (mal-env-set env* key value)))
            (setq env env*
                  ast form))) ; TCO
         (quote
          (throw 'return a1))
         (quasiquote
          (setq ast (quasiquote a1))) ; TCO
         (defmacro!
          (let ((identifier (mal-value a1))
                (value (mal-macro (EVAL a2 env))))
            (throw 'return (mal-env-set env identifier value))))
         (try*
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
               (signal (car err) (cdr err))))))
         (do
          (let* ((a0... (cdr a))
                 (butlast (butlast a0...))
                 (last (car (last a0...))))
            (mapcar (lambda (item) (EVAL item env)) butlast)
            (setq ast last))) ; TCO
         (if
          (let* ((condition (EVAL a1 env))
                 (condition-type (mal-type condition))
                 (then a2)
                 (else a3))
            (if (and (not (eq condition-type 'false))
                     (not (eq condition-type 'nil)))
                (setq ast then) ; TCO
              (if else
                  (setq ast else) ; TCO
                (throw 'return mal-nil)))))
         (fn*
          (let* ((binds (mapcar 'mal-value (mal-value a1)))
                 (body a2)
                 (fn (mal-fn
                      (lambda (&rest args)
                        (let ((env* (mal-env env binds args)))
                          (EVAL body env*))))))
            (throw 'return (mal-func body binds env fn))))
         (t
          ;; not a special form
          (let ((fn (EVAL (car a) env))
                (args (cdr a)))
            (if (mal-func-p fn)
              (if (mal-func-macro-p fn)
                (setq ast (apply (mal-value (mal-func-fn fn)) args)) ; TCO
                (let ((env* (mal-env (mal-func-env fn)
                                     (mal-func-params fn)
                                     (mapcar (lambda (x) (EVAL x env)) args))))
                  (setq env env*
                        ast (mal-func-ast fn)))) ; TCO
              ;; built-in function
              (let ((fn* (mal-value fn)))
                (throw 'return (apply fn* (mapcar (lambda (x) (EVAL x env))
                                                  args))))))))))
     (symbol
      (let ((key (mal-value ast)))
        (throw 'return (or (mal-env-get env key)
                           (error "'%s' not found" key)))))
     (vector
      (throw 'return
             (mal-vector (vconcat (mapcar (lambda (item) (EVAL item env))
                                          (mal-value ast))))))
     (map
      (let ((map (copy-hash-table (mal-value ast))))
        (maphash (lambda (key val)
                   (puthash key (EVAL val env) map))
                 map)
        (throw 'return (mal-map map))))
     (t
      ;; return as is
      (throw 'return ast))))))

(mal-env-set repl-env 'eval (mal-fn (let ((env repl-env)) (lambda (form) (EVAL form env)))))
(mal-env-set repl-env '*ARGV* (mal-list (mapcar 'mal-string (cdr argv))))
(mal-env-set repl-env '*host-language* (mal-string "elisp"))

(defun PRINT (input)
  (pr-str input t))

(defun rep (input)
  (PRINT (EVAL (READ input) repl-env)))

(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
(rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")

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
      (princ (format "Expected '%c', got EOF\n"
                     (cl-case (cadr err)
                       (string ?\")
                       (list   ?\))
                       (vector ?\])
                       (map    ?})))))
     (error ; catch-all
      (println (error-message-string err)))))

(defun main ()
  (if argv
      (with-error-handling
       (rep (format "(load-file \"%s\")" (car argv))))
    (let (eof)
      (rep "(println (str \"Mal [\" *host-language* \"]\"))")
      (while (not eof)
        (let ((input (readln "user> ")))
          (if input
              (with-error-handling
               (println (rep input)))
            (setq eof t)
            ;; print final newline
            (terpri)))))))

(main)
