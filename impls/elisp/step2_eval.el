;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'mal/types)
(require 'mal/reader)
(require 'mal/printer)


(defun READ (input)
  (read-str input))

(defun EVAL (ast env)
  (let (a)

     ;; (println "EVAL: %s\n" (PRINT ast))

     (cond

     ((setq a (mal-list-value ast))
      (let ((fn* (mal-fn-core-value (EVAL (car a) env)))
            (args (mapcar (lambda (x) (EVAL x env)) (cdr a))))
        (apply fn*  args)))
     ((setq a (mal-symbol-value ast))
      (or (gethash a env) (error "'%s' not found" a)))
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
  (defvar repl-env (make-hash-table :test 'eq))

  (dolist (binding
           '((+ . (lambda (a b) (mal-number (+ (mal-number-value a) (mal-number-value b)))))
             (- . (lambda (a b) (mal-number (- (mal-number-value a) (mal-number-value b)))))
             (* . (lambda (a b) (mal-number (* (mal-number-value a) (mal-number-value b)))))
             (/ . (lambda (a b) (mal-number (/ (mal-number-value a) (mal-number-value b)))))))
    (let ((symbol (car binding))
          (fn (cdr binding)))
      (puthash symbol (mal-fn-core fn) repl-env)))

  (let (input)
    (while (setq input (readln "user> "))
      (with-error-handling
       (println (rep input repl-env))))
    ;; print final newline
    (terpri)))

(main)
