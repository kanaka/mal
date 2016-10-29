(defpackage :mal
  (:use :common-lisp
        :types
        :env
        :reader
        :printer
        :genhash)
  (:export :main))

(in-package :mal)

(defvar *repl-env* (env:create-mal-env))

(env:set-env *repl-env*
             (types:make-mal-symbol "+")
             (types:make-mal-builtin-fn (lambda (value1 value2)
                                          (types:apply-unwrapped-values '+
                                                                        value1
                                                                        value2))))

(env:set-env *repl-env*
             (types:make-mal-symbol "-")
             (types:make-mal-builtin-fn (lambda (value1 value2)
                                          (types:apply-unwrapped-values '-
                                                                        value1
                                                                        value2))))

(env:set-env *repl-env*
             (types:make-mal-symbol "*")
             (types:make-mal-builtin-fn (lambda (value1 value2)
                                          (types:apply-unwrapped-values '*
                                                                        value1
                                                                        value2))))

(env:set-env *repl-env*
             (types:make-mal-symbol "/")
             (types:make-mal-builtin-fn (lambda (value1 value2)
                                          (types:apply-unwrapped-values '/
                                                                        value1
                                                                        value2))))

(defvar mal-def! (make-mal-symbol "def!"))
(defvar mal-let* (make-mal-symbol "let*"))

(defun eval-sequence (sequence env)
  (map 'list
       (lambda (ast) (mal-eval ast env))
       (types:mal-data-value sequence)))

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
    (types:hash-map (eval-hash-map ast env ))
    (types:any ast)))

(defun eval-let* (forms env)
  (let ((new-env (env:create-mal-env :parent env))
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
      ((mal-data-value= mal-def! (first forms))
       (env:set-env env (second forms) (mal-eval (third forms) env)))
      ((mal-data-value= mal-let* (first forms))
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
    (error (condition)
      (format nil
              "~a"
              condition))))

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

(defun main (&optional (argv nil argv-provided-p))
  (declare (ignorable argv argv-provided-p))
  (setf *use-readline-p* (not (or (string= (uiop:getenv "PERL_RL") "false")
                                  (string= (uiop:getenv "TERM") "dumb"))))
  (loop do (let ((line (mal-readline "user> ")))
             (if line (mal-writeline (rep line)) (return)))))
