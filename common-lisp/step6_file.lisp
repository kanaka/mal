(defpackage :mal
  (:use :common-lisp
        :types
        :env
        :reader
        :printer
        :core
        :utils)
  (:export :main))

(in-package :mal)

(defvar *repl-env* (env:create-mal-env))

(dolist (binding core:ns)
  (env:set-env *repl-env*
               (car binding)
               (cdr binding)))

(defvar mal-def! (make-mal-symbol "def!"))
(defvar mal-let* (make-mal-symbol "let*"))
(defvar mal-do (make-mal-symbol "do"))
(defvar mal-if (make-mal-symbol "if"))
(defvar mal-fn* (make-mal-symbol "fn*"))

(defun eval-sequence (sequence env)
  (map 'list
       (lambda (ast) (mal-eval ast env))
       (mal-data-value sequence)))

(defun eval-hash-map (hash-map env)
  (let ((hash-map-value (types:mal-data-value hash-map))
        (new-hash-table (types:make-mal-value-hash-table)))
    (genhash:hashmap (lambda (key value)
                       (setf (genhash:hashref (mal-eval key env) new-hash-table)
                             (mal-eval value env)))
                     hash-map-value)
    (types:make-mal-hash-map new-hash-table)))

(defun eval-ast (ast env)
  (switch-mal-type ast
    (types:symbol (env:get-env env ast))
    (types:list (eval-sequence ast env))
    (types:vector (make-mal-vector (apply 'vector (eval-sequence ast env))))
    (types:hash-map (eval-hash-map ast env))
    (types:any ast)))

(defun mal-read (string)
  (reader:read-str string))

(defun mal-eval (ast env)
  (loop
     do (cond
          ((null ast) (return types:mal-nil))
          ((not (types:mal-list-p ast)) (return (eval-ast ast env)))
          ((zerop (length (mal-data-value ast))) (return ast))
          (t (let ((forms (mal-data-value ast)))
               (cond
                 ((mal-data-value= mal-def! (first forms))
                  (return (env:set-env env (second forms) (mal-eval (third forms) env))))

                 ((mal-data-value= mal-let* (first forms))
                  (let ((new-env (env:create-mal-env :parent env))
                        ;; Convert a potential vector to a list
                        (bindings (map 'list
                                       #'identity
                                       (mal-data-value (second forms)))))

                    (mapcar (lambda (binding)
                              (env:set-env new-env
                                           (car binding)
                                           (mal-eval (or (cdr binding)
                                                         types:mal-nil)
                                                     new-env)))
                            (loop
                               for (symbol value) on bindings
                               by #'cddr
                               collect (cons symbol value)))
                    (setf ast (third forms)
                          env new-env)))

                 ((mal-data-value= mal-do (first forms))
                  (mapc (lambda (form) (mal-eval form env))
                        (butlast (cdr forms)))
                  (setf ast (car (last forms))))

                 ((mal-data-value= mal-if (first forms))
                  (let ((predicate (mal-eval (second forms) env)))
                    (setf ast (if (or (mal-data-value= predicate types:mal-nil)
                                      (mal-data-value= predicate types:mal-false))
                                  (fourth forms)
                                  (third forms)))))

                 ((mal-data-value= mal-fn* (first forms))
                  (return (let ((arglist (second forms))
                                (body (third forms)))
                            (types:make-mal-fn (lambda (&rest args)
                                                 (mal-eval body (env:create-mal-env :parent env
                                                                                    :binds (map 'list
                                                                                                #'identity
                                                                                                (mal-data-value arglist))
                                                                                    :exprs args)))
                                               :attrs (list (cons 'params arglist)
                                                            (cons 'ast body)
                                                            (cons 'env env))))))

                 (t (let* ((evaluated-list (eval-ast ast env))
                           (function (car evaluated-list)))
                      ;; If first element is a mal function unwrap it
                      (if (not (types:mal-fn-p function))
                          (return (apply (mal-data-value function)
                                         (cdr evaluated-list)))
                          (let* ((attrs (types:mal-data-attrs function)))
                            (setf ast (cdr (assoc 'ast attrs))
                                  env (env:create-mal-env :parent (cdr (assoc 'env attrs))
                                                          :binds (map 'list
                                                                      #'identity
                                                                      (mal-data-value (cdr (assoc 'params attrs))))
                                                          :exprs (cdr evaluated-list)))))))))))))

(defun mal-print (expression)
  (printer:pr-str expression))

(defun rep (string)
  (handler-case
      (mal-print (mal-eval (mal-read string)
                           *repl-env*))
    (error (condition)
      (format nil
              "~a"
              condition))))

(env:set-env *repl-env*
             (types:make-mal-symbol "eval")
             (types:make-mal-builtin-fn (lambda (ast)
                                          (mal-eval ast *repl-env*))))

(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")

(defvar *use-readline-p* nil)

(defun raw-input (prompt)
  (format *standard-output* prompt)
  (force-output *standard-output*)
  (read-line *standard-input* nil))

(defun mal-readline (prompt)
  (if *use-readline-p*
      (cl-readline:readline :prompt prompt
                            :add-history t
                            :novelty-check (lambda (old new)
                                             (not (string= old new))))
      (raw-input prompt)))

(defun mal-writeline (string)
  (when string
    (write-line string)
    (force-output *standard-output*)))

(defun repl ()
  (loop do (let ((line (mal-readline "user> ")))
             (if line
                 (mal-writeline (rep line))
                 (return)))))

(defun run-file (file)
  (rep (format nil "(load-file \"~a\")" file)))

(defun main (&optional (argv nil argv-provided-p))
  (setf *use-readline-p* (not (or (string= (uiop:getenv "PERL_RL") "false")
                                  (string= (uiop:getenv "TERM") "dumb"))))

  (let ((args (if argv-provided-p
                  argv
                  (cdr (utils:raw-command-line-arguments)))))
    (env:set-env *repl-env*
                 (types:make-mal-symbol "*ARGV*")
                 (types:wrap-value (cdr args) :listp t))
    (if (null args)
        (repl)
        (run-file (car args)))))
