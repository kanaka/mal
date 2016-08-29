(require "dependencies")

(defpackage :mal
  (:use :common-lisp
        :readline
        :types
        :env
        :reader
        :printer))

(in-package :mal)

(defvar *repl-env* (make-instance 'env:mal-environment))

(set-env *repl-env*
         (types:make-mal-symbol "+")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (apply-unwrapped-values '+ value1 value2))))

(set-env *repl-env*
         (types:make-mal-symbol "-")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (apply-unwrapped-values '- value1 value2))))

(set-env *repl-env*
         (types:make-mal-symbol "*")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (apply-unwrapped-values '* value1 value2))))

(set-env *repl-env*
         (types:make-mal-symbol "/")
         (types:make-mal-builtin-fn (lambda (value1 value2)
                                      (apply-unwrapped-values '/ value1 value2))))

(defvar mal-def! (make-mal-symbol "def!"))
(defvar mal-let* (make-mal-symbol "let*"))

(defun eval-sequence (sequence env)
  (map 'list
       (lambda (ast) (mal-eval ast env))
       (mal-data-value sequence)))

(defun eval-hash-map (hash-map env)
  (let ((hash-map-value (mal-data-value hash-map))
        (new-hash-table (make-hash-table :test 'types:mal-value=)))
    (loop
       for key being the hash-keys of hash-map-value
       do (setf (gethash key new-hash-table)
                (mal-eval (gethash key hash-map-value) env)))
    (make-mal-hash-map new-hash-table)))

(defun eval-ast (ast env)
  (switch-mal-type ast
    (types:symbol (env:get-env env ast))
    (types:list (eval-sequence ast env))
    (types:vector (make-mal-vector (apply 'vector (eval-sequence ast env))))
    (types:hash-map (eval-hash-map ast env ))
    (types:any ast)))

(defun eval-let* (forms env)
  (let ((new-env (make-instance 'env:mal-environment
                                :parent env))
        ;; Convert a potential vector to a list
        (bindings (map 'list
                       #'identity
                       (types:mal-data-value (second forms)))))

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

    (mal-eval (third forms) new-env)))

(defun eval-list (ast env)
  (let ((forms (mal-data-value ast)))
    (cond
      ((mal-value= mal-def! (first forms))
       (env:set-env env (second forms) (mal-eval (third forms) env)))
      ((mal-value= mal-let* (first forms))
       (eval-let* forms env))
      (t (let ((evaluated-list (eval-ast ast env)))
           (apply (types:mal-data-value (car evaluated-list))
                  (cdr evaluated-list)))))))

(defun mal-read (string)
  (reader:read-str string))

(defun mal-eval (ast env)
  (cond
    ((null ast) types:mal-nil)
    ((not (types:mal-list-p ast)) (eval-ast ast env))
    ((zerop (length (mal-data-value ast))) ast)
    (t (eval-list ast env))))

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
              condition))))

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

(defun main ()
  (loop do (let ((line (mal-readline "user> ")))
             (if line
                 (mal-writeline (rep line))
                 (return))))
  (when use-readline-p
    (save-history)))

;; Do not start REPL inside Emacs
(unless (member :swank *features*)
  (main))
