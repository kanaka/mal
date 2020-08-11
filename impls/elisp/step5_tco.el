;; -*- lexical-binding: t; -*-

(setq debug-on-error t)
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

(defun READ (input)
  (read-str input))

(defun EVAL (ast env)
  (catch 'return
    (while t
      (if (and (mal-list-p ast) (mal-value ast))
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
                  (let ((fn* (mal-value fn)))
                    (throw 'return (apply fn* args))))))))
        (throw 'return (eval-ast ast env))))))

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

(rep "(def! not (fn* (a) (if a false true)))")

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
