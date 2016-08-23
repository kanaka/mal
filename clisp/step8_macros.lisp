(require "reader")
(require "printer")
(require "types")
(require "env")
(require "core")

(defpackage :mal
  (:use :common-lisp
        :types
        :env
        :reader
        :printer
        :core))

(in-package :mal)

(define-condition invalid-function (types:mal-error)
  ((form :initarg :form :reader form)
   (context :initarg :context :reader context))
  (:report (lambda (condition stream)
             (format stream
                     "Invalid function '~a' provided while ~a"
                     (printer:pr-str (form condition))
                     (if (string= (context condition) "apply")
                         "applying"
                         "defining macro")))))

(defvar *repl-env* (make-instance 'env:mal-environment))

(dolist (binding core:ns)
  (env:set-env *repl-env*
               (car binding)
               (cdr binding)))

(env:set-env *repl-env*
             (types:make-mal-symbol '|eval|)
             (types:make-mal-builtin-fn (lambda (ast)
                                          (mal-eval ast *repl-env*))))

(defun eval-sequence (sequence env)
  (map 'list
       (lambda (ast) (mal-eval ast env))
       (mal-value sequence)))

(defun eval-hash-map (hash-map env)
  (let ((hash-map-value (mal-value hash-map))
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

(defun is-pair (value)
  (and (or (mal-list-p value)
           (mal-vector-p value))
       (not (zerop (length (mal-value value))))))

(defun quasiquote (ast)
  (if (not (is-pair ast))
      (types:make-mal-list (list (types:make-mal-symbol '|quote|)
                                 ast))
      (let ((forms (map 'list #'identity (mal-value ast))))
        (cond
          ((mal-value= (make-mal-symbol '|unquote|) (first forms))
           (second forms))

          ((and (is-pair (first forms))
                (mal-value= (make-mal-symbol '|splice-unquote|)
                            (first (mal-value (first forms)))))
           (types:make-mal-list (list (types:make-mal-symbol '|concat|)
                                      (second (mal-value (first forms)))
                                      (quasiquote (make-mal-list (cdr forms))))))

          (t (types:make-mal-list (list (types:make-mal-symbol '|cons|)
                                        (quasiquote (first forms))
                                        (quasiquote (make-mal-list (cdr forms))))))))))

(defun is-macro-call (ast env)
  (when (and (types:mal-list-p ast)
             (not (zerop (length (mal-value ast)))))
    (let* ((func-symbol (first (mal-value ast)))
           (func (when (types:mal-symbol-p func-symbol)
                   (ignore-errors (env:get-env env func-symbol)))))
      (and func
           (types:mal-fn-p func)
           (cdr (assoc 'is-macro (types:mal-attrs func)))))))

(defun mal-macroexpand (ast env)
  (loop
     while (is-macro-call ast env)
     do (let* ((forms (types:mal-value ast))
               (func (env:get-env env (first forms))))
          (setf ast (apply (mal-value func)
                           (cdr forms)))))
  ast)

(defun mal-eval (ast env)
  (loop
     do (setf ast (mal-macroexpand ast env))
     do (cond
          ((null ast) (return (make-mal-nil nil)))
          ((not (types:mal-list-p ast)) (return (eval-ast ast env)))
          ((zerop (length (mal-value ast))) (return ast))
          (t (let ((forms (mal-value ast)))
               (cond
                 ((mal-value= (make-mal-symbol '|quote|) (first forms))
                  (return (second forms)))

                 ((mal-value= (make-mal-symbol '|quasiquote|) (first forms))
                  (setf ast (quasiquote (second forms))))

                 ((mal-value= (make-mal-symbol '|macroexpand|) (first forms))
                  (return (mal-macroexpand (second forms) env)))

                 ((mal-value= (make-mal-symbol '|def!|) (first forms))
                  (return (env:set-env env (second forms) (mal-eval (third forms) env))))

                 ((mal-value= (make-mal-symbol '|defmacro!|) (first forms))
                  (let ((value (mal-eval (third forms) env)))
                    (return (if (types:mal-fn-p value)
                                (env:set-env env
                                             (second forms)
                                             (progn
                                               (setf (cdr (assoc 'is-macro (types:mal-attrs value))) t)
                                               value))
                                (error 'invalid-function
                                       :form value
                                       :context "macro")))))

                 ((mal-value= (make-mal-symbol '|let*|) (first forms))
                  (let ((new-env (make-instance 'env:mal-environment
                                                :parent env))
                        ;; Convert a potential vector to a list
                        (bindings (map 'list
                                       #'identity
                                       (mal-value (second forms)))))

                    (mapcar (lambda (binding)
                              (env:set-env new-env
                                           (car binding)
                                           (mal-eval (or (cdr binding)
                                                         (types:make-mal-nil nil))
                                                     new-env)))
                            (loop
                               for (symbol value) on bindings
                               by #'cddr
                               collect (cons symbol value)))
                    (setf ast (third forms)
                          env new-env)))

                 ((mal-value= (make-mal-symbol '|do|) (first forms))
                  (mapc (lambda (form) (mal-eval form env))
                        (butlast (cdr forms)))
                  (setf ast (car (last forms))))

                 ((mal-value= (make-mal-symbol '|if|) (first forms))
                  (let ((predicate (mal-eval (second forms) env)))
                    (setf ast (if (or (mal-value= predicate (types:make-mal-nil nil))
                                      (mal-value= predicate (types:make-mal-boolean nil)))
                                  (fourth forms)
                                  (third forms)))))

                 ((mal-value= (make-mal-symbol '|fn*|) (first forms))
                  (return (let ((arglist (second forms))
                                (body (third forms)))
                            (types:make-mal-fn (lambda (&rest args)
                                                 (mal-eval body (make-instance 'env:mal-environment
                                                                               :parent env
                                                                               :binds (map 'list
                                                                                           #'identity
                                                                                           (mal-value arglist))
                                                                               :exprs args)))
                                               :attrs (list (cons 'params arglist)
                                                            (cons 'ast body)
                                                            (cons 'env env)
                                                            (cons 'is-macro nil))))))

                 (t (let* ((evaluated-list (eval-ast ast env))
                           (function (car evaluated-list)))
                      ;; If first element is a mal function unwrap it
                      (cond ((types:mal-fn-p function)
                             (let* ((attrs (types:mal-attrs function)))
                               (setf ast (cdr (assoc 'ast attrs))
                                     env (make-instance 'env:mal-environment
                                                        :parent (cdr (assoc 'env attrs))
                                                        :binds (map 'list
                                                                    #'identity
                                                                    (mal-value (cdr (assoc 'params attrs))))
                                                        :exprs (cdr evaluated-list)))))
                            ((types:mal-builtin-fn-p function)
                             (return (apply (mal-value function)
                                            (cdr evaluated-list))))
                            (t (error 'invalid-function
                                      :form function
                                      :context "apply")))))))))))

(defun mal-read (string)
  (reader:read-str string))

(defun mal-print (expression)
  (printer:pr-str expression))

(defun rep (string)
  (handler-case
      (mal-print (mal-eval (mal-read string)
                           *repl-env*))
    (types:mal-error (condition)
      (format nil
              "~a"
              condition))
    (error (condition)
      (format nil
              "Internal error: ~a"
              condition))))

(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
(rep "(def! *ARGV* (list))")
(rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
(rep "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))")

(defun readline (prompt &optional (in-stream *standard-input*) (out-stream *standard-output*))
  (format out-stream prompt)
  (force-output out-stream)
  (read-line in-stream nil))

(defun writeline (string)
  (when string
    (write-line string)))

(defun main ()
  (loop do (let ((line (readline "user> ")))
             (if line (writeline (rep line)) (return)))))

(env:set-env *repl-env*
             (types:make-mal-symbol '|*ARGV*|)
             (types:wrap-value (cdr common-lisp-user::*args*)
                               :listp t))

(if (null common-lisp-user::*args*)
    (main)
    (rep (format nil
                 "(load-file \"~a\")"
                 (car common-lisp-user::*args*))))
