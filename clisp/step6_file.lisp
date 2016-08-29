(require "dependencies")

(defpackage :mal
  (:use :common-lisp
        :readline
        :types
        :env
        :reader
        :printer
        :core))

(in-package :mal)

(defvar *repl-env* (make-instance 'env:mal-environment))

(dolist (binding core:ns)
  (env:set-env *repl-env*
               (car binding)
               (cdr binding)))

(defvar mal-def! (make-mal-symbol "def!"))
(defvar mal-let* (make-mal-symbol "let*"))
(defvar mal-do (make-mal-symbol "do"))
(defvar mal-if (make-mal-symbol "if"))
(defvar mal-fn* (make-mal-symbol "fn*"))

(env:set-env *repl-env*
             (types:make-mal-symbol "eval")
             (types:make-mal-builtin-fn (lambda (ast)
                                          (mal-eval ast *repl-env*))))

(defun eval-sequence (sequence env)
  (map 'list
       (lambda (ast) (mal-eval ast env))
       (mal-data-value sequence)))

(defun eval-hash-map (hash-map env)
  (let ((hash-map-value (mal-data-value hash-map))
        (new-hash-table (make-hash-table :test 'types:mal-value=)))
    (loop
       for key being the hash-keys of hash-map-value
       do (setf (gethash (mal-eval key env) new-hash-table)
                (mal-eval (gethash key hash-map-value) env)))
    (make-mal-hash-map new-hash-table)))

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
                 ((mal-value= mal-def! (first forms))
                  (return (env:set-env env (second forms) (mal-eval (third forms) env))))

                 ((mal-value= mal-let* (first forms))
                  (let ((new-env (make-instance 'env:mal-environment
                                                :parent env))
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

                 ((mal-value= mal-do (first forms))
                  (mapc (lambda (form) (mal-eval form env))
                        (butlast (cdr forms)))
                  (setf ast (car (last forms))))

                 ((mal-value= mal-if (first forms))
                  (let ((predicate (mal-eval (second forms) env)))
                    (setf ast (if (or (mal-value= predicate types:mal-nil)
                                      (mal-value= predicate types:mal-false))
                                  (fourth forms)
                                  (third forms)))))

                 ((mal-value= mal-fn* (first forms))
                  (return (let ((arglist (second forms))
                                (body (third forms)))
                            (types:make-mal-fn (lambda (&rest args)
                                                 (mal-eval body (make-instance 'env:mal-environment
                                                                               :parent env
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
                                  env (make-instance 'env:mal-environment
                                                     :parent (cdr (assoc 'env attrs))
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
    (reader:eof (condition)
      (format nil
              "~a"
              condition))
    (env:undefined-symbol (condition)
      (format nil
              "~a"
              condition))
    (error (condition)
      (format nil
              "~a"
              condition))))

(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
(rep "(def! *ARGV* (list))")

(env:set-env *repl-env*
             (types:make-mal-symbol "*ARGV*")
             (types:wrap-value (cdr common-lisp-user::*args*)
                               :listp t))

;; Readline setup
;;; The test runner sets this environment variable, in which case we do
;;; use readline since tests do not work with the readline interface
(defvar use-readline-p (not (string= (ext:getenv "PERL_RL") "false")))

(defvar *history-file* (namestring (merge-pathnames (user-homedir-pathname)
                                                         ".mal-clisp-history")))

(defun load-history ()
  (readline:read-history *history-file*))

(defun save-history ()
  (readline:write-history *history-file*))

;; Setup history
(when use-readline-p
  (load-history))

(defun raw-input (prompt)
  (format *standard-output* prompt)
  (force-output *standard-output*)
  (read-line *standard-input* nil))

(defun mal-readline (prompt)
  (let ((input (if use-readline-p
                   (readline:readline prompt)
                   (raw-input prompt))))
    (when (and use-readline-p
               input
               (not (zerop (length input))))
      (readline:add-history input))
    input))

(defun mal-writeline (string)
  (when string
    (write-line string)))

(defun repl ()
  (loop do (let ((line (mal-readline "user> ")))
             (if line
                 (mal-writeline (rep line))
                 (return))))
  (when use-readline-p
    (save-history)))

(defun main ()
  (if (null common-lisp-user::*args*)
      ;; Do not start REPL inside Emacs
      (unless (member :swank *features*)
        (repl))
      (rep (format nil
                   "(load-file \"~a\")"
                   (car common-lisp-user::*args*)))))

(main)
