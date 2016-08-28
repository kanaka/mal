(require "reader")
(require "printer")
(require "types")
(require "env")

(defpackage :mal
  (:use :common-lisp :types :env :reader :printer))

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
                                         (types:make-mal-nil nil))
                                     new-env)))
            (loop
               for (symbol value) on bindings
               by #'cddr
               collect (cons symbol value)))

    (mal-eval (third forms) new-env)))

(defun eval-list (ast env)
  (let ((forms (mal-data-value ast)))
    (cond
      ((mal-value= (make-mal-symbol "def!") (first forms))
       (env:set-env env (second forms) (mal-eval (third forms) env)))
      ((mal-value= (make-mal-symbol "let*") (first forms))
       (eval-let* forms env))
      (t (let ((evaluated-list (eval-ast ast env)))
           (apply (types:mal-data-value (car evaluated-list))
                  (cdr evaluated-list)))))))

(defun mal-read (string)
  (reader:read-str string))

(defun mal-eval (ast env)
  (cond
    ((null ast) (make-mal-nil nil))
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

(main)
