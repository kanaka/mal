(require "dependencies")

(defpackage :mal
  (:use :common-lisp
        :types
        :env
        :reader
        :printer
        :core))

(in-package :mal)

(define-condition invalid-function (types:mal-runtime-exception)
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

(defvar mal-quote (make-mal-symbol "quote"))
(defvar mal-quasiquote (make-mal-symbol "quasiquote"))
(defvar mal-unquote (make-mal-symbol "unquote"))
(defvar mal-splice-unquote (make-mal-symbol "splice-unquote"))
(defvar mal-cons (make-mal-symbol "cons"))
(defvar mal-concat (make-mal-symbol "concat"))
(defvar mal-macroexpand (make-mal-symbol "macroexpand"))
(defvar mal-def! (make-mal-symbol "def!"))
(defvar mal-defmacro! (make-mal-symbol "defmacro!"))
(defvar mal-let* (make-mal-symbol "let*"))
(defvar mal-do (make-mal-symbol "do"))
(defvar mal-if (make-mal-symbol "if"))
(defvar mal-fn* (make-mal-symbol "fn*"))
(defvar mal-try* (make-mal-symbol "try*"))
(defvar mal-catch* (make-mal-symbol "catch*"))

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

(defun is-pair (value)
  (and (or (mal-list-p value)
           (mal-vector-p value))
       (not (zerop (length (mal-data-value value))))))

(defun quasiquote (ast)
  (if (not (is-pair ast))
      (types:make-mal-list (list mal-quote ast))
      (let ((forms (map 'list #'identity (mal-data-value ast))))
        (cond
          ((mal-value= mal-unquote (first forms))
           (second forms))

          ((and (is-pair (first forms))
                (mal-value= mal-splice-unquote
                            (first (mal-data-value (first forms)))))
           (types:make-mal-list (list mal-concat
                                      (second (mal-data-value (first forms)))
                                      (quasiquote (make-mal-list (cdr forms))))))

          (t (types:make-mal-list (list mal-cons
                                        (quasiquote (first forms))
                                        (quasiquote (make-mal-list (cdr forms))))))))))

(defun is-macro-call (ast env)
  (when (and (types:mal-list-p ast)
             (not (zerop (length (mal-data-value ast)))))
    (let* ((func-symbol (first (mal-data-value ast)))
           (func (when (types:mal-symbol-p func-symbol)
                   (env:find-env env func-symbol))))
      (and func
           (types:mal-fn-p func)
           (cdr (assoc 'is-macro (types:mal-data-attrs func)))))))

(defun mal-macroexpand (ast env)
  (loop
     while (is-macro-call ast env)
     do (let* ((forms (types:mal-data-value ast))
               (func (env:get-env env (first forms))))
          (setf ast (apply (mal-data-value func)
                           (cdr forms)))))
  ast)

(defun mal-eval (ast env)
  (loop
     do (setf ast (mal-macroexpand ast env))
     do (cond
          ((null ast) (return (make-mal-nil nil)))
          ((not (types:mal-list-p ast)) (return (eval-ast ast env)))
          ((zerop (length (mal-data-value ast))) (return ast))
          (t (let ((forms (mal-data-value ast)))
               (cond
                 ((mal-value= mal-quote (first forms))
                  (return (second forms)))

                 ((mal-value= mal-quasiquote (first forms))
                  (setf ast (quasiquote (second forms))))

                 ((mal-value= mal-macroexpand (first forms))
                  (return (mal-macroexpand (second forms) env)))

                 ((mal-value= mal-def! (first forms))
                  (return (env:set-env env (second forms) (mal-eval (third forms) env))))

                 ((mal-value= mal-defmacro! (first forms))
                  (let ((value (mal-eval (third forms) env)))
                    (return (if (types:mal-fn-p value)
                                (env:set-env env
                                             (second forms)
                                             (progn
                                               (setf (cdr (assoc 'is-macro (types:mal-data-attrs value))) t)
                                               value))
                                (error 'invalid-function
                                       :form value
                                       :context "macro")))))

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
                                                         (types:make-mal-nil nil))
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
                    (setf ast (if (or (mal-value= predicate (types:make-mal-nil nil))
                                      (mal-value= predicate (types:make-mal-boolean nil)))
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
                                                            (cons 'env env)
                                                            (cons 'is-macro nil))))))

                 ((mal-value= mal-try* (first forms))
                  (handler-case
                      (return (mal-eval (second forms) env))
                    ((or types:mal-exception types:mal-error) (condition)
                      (when (third forms)
                        (let ((catch-forms (types:mal-data-value (third forms))))
                          (when (mal-value= mal-catch*
                                            (first catch-forms))
                            (return (mal-eval (third catch-forms)
                                              (make-instance 'env:mal-environment
                                                             :parent env
                                                             :binds (list (second catch-forms))
                                                             :exprs (list (if (or (typep condition 'types:mal-runtime-exception)
                                                                                  (typep condition 'types:mal-error))
                                                                              (types:make-mal-string (format nil "~a" condition))
                                                                              (types::mal-exception-data condition)))))))))
                     (error condition))))

                 (t (let* ((evaluated-list (eval-ast ast env))
                           (function (car evaluated-list)))
                      ;; If first element is a mal function unwrap it
                      (cond ((types:mal-fn-p function)
                             (let* ((attrs (types:mal-data-attrs function)))
                               (setf ast (cdr (assoc 'ast attrs))
                                     env (make-instance 'env:mal-environment
                                                        :parent (cdr (assoc 'env attrs))
                                                        :binds (map 'list
                                                                    #'identity
                                                                    (mal-data-value (cdr (assoc 'params attrs))))
                                                        :exprs (cdr evaluated-list)))))
                            ((types:mal-builtin-fn-p function)
                             (return (apply (mal-data-value function)
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
              "Error: ~a"
              condition))
    (types:mal-runtime-exception (condition)
      (format nil
              "Exception: ~a"
              condition))
    (types:mal-user-exception (condition)
      (format nil
              "Exception: ~a"
              (pr-str (types::mal-exception-data condition))))
    (error (condition)
      (format nil
              "Internal error: ~a"
              condition))))

(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
(rep "(def! *ARGV* (list))")
(rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
(rep "(def! *host-language* \"clisp\")")
(rep "(def! *gensym-counter* (atom 0))")
(rep "(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))")
(rep "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))")

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
             (types:make-mal-symbol "*ARGV*")
             (types:wrap-value (cdr common-lisp-user::*args*)
                               :listp t))

(if (null common-lisp-user::*args*)
    (main)
    (rep (format nil
                 "(load-file \"~a\")"
                 (car common-lisp-user::*args*))))
