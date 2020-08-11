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
  (let ((s (car (mal-value ast))))
    (and (mal-symbol-p s)
         (eq (mal-value s) sym))))

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

(defun MACROEXPAND (ast env)
  (let (a a0 macro)
    (while (and (mal-list-p ast)
                (setq a (mal-value ast))
                (setq a0 (car a))
                (mal-symbol-p a0)
                (setq macro (mal-env-find env (mal-value a0)))
                (mal-func-p macro)
                (mal-func-macro-p macro))
      (setq ast (apply (mal-value (mal-func-fn macro)) (cdr a)))))
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
             (a1 (cadr a))
             (a2 (nth 2 a))
             (a3 (nth 3 a)))
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
         (quasiquoteexpand
          (throw 'return (quasiquote a1)))
         (quasiquote
          (setq ast (quasiquote a1))) ; TCO
         (defmacro!
          (let ((identifier (mal-value a1))
                (value (EVAL a2 env)))
            (setf (aref (aref value 1) 4) t)
            (throw 'return (mal-env-set env identifier value))))
         (macroexpand
          (throw 'return (MACROEXPAND a1 env)))
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
            (when butlast
              (eval-ast (mal-list butlast) env))
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
