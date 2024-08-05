;; -*- lexical-binding: t; -*-

(require 'mal/types)
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
       (if a
        (cl-case (mal-value (car a))
         (def!
          (let ((identifier (mal-value a1))
                (value (EVAL a2 env)))
            (mal-env-set env identifier value)))
         (let*
          (let ((env* (mal-env env))
                (bindings (mal-listify a1))
                (form a2))
            (while bindings
              (let ((key (mal-value (pop bindings)))
                    (value (EVAL (pop bindings) env*)))
                (mal-env-set env* key value)))
            (EVAL form env*)))
         (do
           (let* ((a0... (cdr a))
                  (butlast (butlast a0...))
                  (last (car (last a0...))))
             (mapcar (lambda (item) (EVAL item env)) butlast)
             (EVAL last env)))
         (if
          (let* ((condition (EVAL a1 env))
                 (condition-type (mal-type condition))
                 (then a2)
                 (else a3))
            (if (and (not (eq condition-type 'false))
                     (not (eq condition-type 'nil)))
                (EVAL then env)
              (if else
                  (EVAL else env)
                mal-nil))))
         (fn*
          (let ((binds (mapcar 'mal-value (mal-value a1)))
                (body a2))
            (mal-fn
             (lambda (&rest args)
               (let ((env* (mal-env env binds args)))
                 (EVAL body env*))))))
         (t
          ;; not a special form
          (let ((fn* (mal-value (EVAL (car a) env)))
                (args (mapcar (lambda (x) (EVAL x env)) (cdr a))))
            (apply fn* args))))
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
