;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'mal/types)
(require 'mal/env)
(require 'mal/reader)
(require 'mal/printer)
(require 'mal/core)

(defun READ (input)
  (read-str input))

(defun EVAL (ast env)
  (let (a)

     (let ((dbgeval (mal-env-get env 'DEBUG-EVAL)))
       (if (not (memq dbgeval (list nil mal-nil mal-false)))
         (println "EVAL: %s\n" (PRINT ast))))

     (cond

     ((setq a (mal-list-value ast))
        (cl-case (mal-symbol-value (car a))
         (def!
           (let ((identifier (mal-symbol-value (cadr a)))
                 (value (EVAL (caddr a) env)))
             (mal-env-set env identifier value)))
         (let*
             (let ((env* (mal-env env))
                   (bindings (mal-seq-value (cadr a)))
                   (form (caddr a))
                   key)
               (seq-do (lambda (current)
                         (if key
                             (let ((value (EVAL current env*)))
                               (mal-env-set env* key value)
                               (setq key nil))
                           (setq key (mal-symbol-value current))))
                       bindings)
            (EVAL form env*)))
         (do
          (setq a (cdr a))              ; skip 'do
          (while (cdr a)
            (EVAL (pop a) env))
          (EVAL (car a) env))
         (if
          (let ((condition (EVAL (cadr a) env)))
            (if (memq condition (list mal-nil mal-false))
                (if (cdddr a)
                    (EVAL (cadddr a) env)
                  mal-nil)
              (EVAL (caddr a) env))))
         (fn*
          (let ((binds (mapcar 'mal-symbol-value (mal-seq-value (cadr a))))
                (body (caddr a)))
            (mal-func
                      (lambda (&rest args)
                            (EVAL body (mal-env env binds args)))
                      body binds env)))
         (t
          ;; not a special form
          (let ((fn (EVAL (car a) env))
                (args (cdr a))
                fn*)
            (cond
             ((mal-func-value fn)
              (EVAL (mal-func-body fn)
                    (mal-env (mal-func-env fn)
                             (mal-func-params fn)
                             (mapcar (lambda (x) (EVAL x env)) args))))
             ((setq fn* (mal-fn-core-value fn))
              ;; built-in function
              (apply fn* (mapcar (lambda (x) (EVAL x env)) args)))
             (t (error "cannot apply %s" (PRINT ast))))))))
     ((setq a (mal-symbol-value ast))
      (or (mal-env-get env a) (error "'%s' not found" a)))
     ((setq a (mal-vector-value ast))
      (mal-vector (vconcat (mapcar (lambda (item) (EVAL item env)) a))))
     ((setq a (mal-map-value ast))
      (let ((map (copy-hash-table a)))
        (maphash (lambda (key val)
                   (puthash key (EVAL val env) map))
                 map)
        (mal-map map)))
     (t
      ;; return as is
      ast))))

(defun PRINT (input)
  (pr-str input t))

(defun rep (input repl-env)
  (PRINT (EVAL (READ input) repl-env)))

(defun readln (prompt)
  ;; C-d throws an error
  (ignore-errors (read-from-minibuffer prompt)))

(defun println (format-string &rest args)
  (princ (if args
             (apply 'format format-string args)
           format-string))
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
  (defvar repl-env (mal-env))

  (dolist (binding core-ns)
    (let ((symbol (car binding))
          (fn (cdr binding)))
      (mal-env-set repl-env symbol (mal-fn-core fn))))

  (rep "(def! not (fn* (a) (if a false true)))" repl-env)

  (let (input)
    (while (setq input (readln "user> "))
      (with-error-handling
       (println (rep input repl-env))))
    ;; print final newline
    (terpri)))

(main)
