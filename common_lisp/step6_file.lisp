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
       do (setf (gethash key new-hash-table)
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
          ((null ast) (return (make-mal-nil nil)))
          ((not (types:mal-list-p ast)) (return (eval-ast ast env)))
          ((zerop (length (mal-value ast))) (return ast))
          (t (let ((forms (mal-value ast)))
               (cond
                 ((mal-value= (make-mal-symbol '|def!|) (first forms))
                  (return (env:set-env env (second forms) (mal-eval (third forms) env))))

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
                                                            (cons 'env env))))))

                 (t (let* ((evaluated-list (eval-ast ast env))
                           (function (car evaluated-list)))
                      ;; If first element is a mal function unwrap it
                      (if (not (types:mal-fn-p function))
                          (return (apply (mal-value function)
                                         (cdr evaluated-list)))
                          (let* ((attrs (types:mal-attrs function)))
                            (setf ast (cdr (assoc 'ast attrs))
                                  env (make-instance 'env:mal-environment
                                                     :parent (cdr (assoc 'env attrs))
                                                     :binds (map 'list
                                                                 #'identity
                                                                 (mal-value (cdr (assoc 'params attrs))))
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
