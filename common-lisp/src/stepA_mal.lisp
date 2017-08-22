(defpackage :mal
  (:use :common-lisp
        :types
        :env
        :reader
        :printer
        :core)
  (:import-from :genhash
                :hashref
                :hashmap)
  (:import-from :utils
                :listify
                :getenv)
  (:export :main))

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
(defvar mal-quote (make-mal-symbol "quote"))
(defvar mal-quasiquote (make-mal-symbol "quasiquote"))
(defvar mal-unquote (make-mal-symbol "unquote"))
(defvar mal-splice-unquote (make-mal-symbol "splice-unquote"))
(defvar mal-cons (make-mal-symbol "cons"))
(defvar mal-concat (make-mal-symbol "concat"))
(defvar mal-defmacro! (make-mal-symbol "defmacro!"))
(defvar mal-macroexpand (make-mal-symbol "macroexpand"))
(defvar mal-try* (make-mal-symbol "try*"))
(defvar mal-catch* (make-mal-symbol "catch*"))
(defvar mal-throw (make-mal-symbol "throw"))

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
    (types:hash-map (eval-hash-map ast env))
    (types:any ast)))

(defun is-pair (value)
  (and (or (mal-list-p value)
           (mal-vector-p value))
       (< 0 (length (types:mal-data-value value)))))

(defun quasiquote (ast)
  (if (not (is-pair ast))
      (types:make-mal-list (list mal-quote ast))
      (let ((forms (map 'list #'identity (types:mal-data-value ast))))
        (cond
          ((types:mal-data-value= mal-unquote (first forms))
           (second forms))

          ((and (is-pair (first forms))
                (types:mal-data-value= mal-splice-unquote
                            (first (types:mal-data-value (first forms)))))
           (types:make-mal-list (list mal-concat
                                      (second (types:mal-data-value (first forms)))
                                      (quasiquote (make-mal-list (cdr forms))))))

          (t (types:make-mal-list (list mal-cons
                                        (quasiquote (first forms))
                                        (quasiquote (make-mal-list (cdr forms))))))))))

(defun is-macro-call (ast env)
  (when (types:mal-list-p ast)
    (let* ((func-symbol (first (types:mal-data-value ast)))
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
          (setf ast (apply (types:mal-data-value func)
                           (cdr forms)))))
  ast)

(defun mal-read (string)
  (reader:read-str string))

(defun mal-eval (ast env)
  (loop
     do (setf ast (mal-macroexpand ast env))
     do (cond
          ((null ast) (return types:mal-nil))
          ((not (types:mal-list-p ast)) (return (eval-ast ast env)))
          ((zerop (length (types:mal-data-value ast))) (return ast))
          (t (let ((forms (types:mal-data-value ast)))
               (cond
                 ((types:mal-data-value= mal-quote (first forms))
                  (return (second forms)))

                 ((types:mal-data-value= mal-quasiquote (first forms))
                  (setf ast (quasiquote (second forms))))

                 ((types:mal-data-value= mal-macroexpand (first forms))
                  (return (mal-macroexpand (second forms) env)))

                 ((types:mal-data-value= mal-def! (first forms))
                  (return (env:set-env env (second forms) (mal-eval (third forms) env))))

                 ((types:mal-data-value= mal-defmacro! (first forms))
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

                 ((types:mal-data-value= mal-let* (first forms))
                  (let ((new-env (env:create-mal-env :parent env))
                        (bindings (utils:listify (types:mal-data-value (second forms)))))

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

                 ((types:mal-data-value= mal-do (first forms))
                  (mapc (lambda (form) (mal-eval form env))
                        (butlast (cdr forms)))
                  (setf ast (car (last forms))))

                 ((types:mal-data-value= mal-if (first forms))
                  (let ((predicate (mal-eval (second forms) env)))
                    (setf ast (if (or (types:mal-data-value= predicate types:mal-nil)
                                      (types:mal-data-value= predicate types:mal-false))
                                  (fourth forms)
                                  (third forms)))))

                 ((types:mal-data-value= mal-fn* (first forms))
                  (return (let ((arglist (second forms))
                                (body (third forms)))
                            (types:make-mal-fn (lambda (&rest args)
                                                 (mal-eval body (env:create-mal-env :parent env
                                                                                    :binds (map 'list
                                                                                                #'identity
                                                                                                (types:mal-data-value arglist))
                                                                                    :exprs args)))
                                               :attrs (list (cons 'params arglist)
                                                            (cons 'ast body)
                                                            (cons 'env env)
                                                            (cons 'is-macro nil))))))

                 ((types:mal-data-value= mal-try* (first forms))
                  (handler-case
                      (return (mal-eval (second forms) env))
                    ((or types:mal-exception types:mal-error) (condition)
                      (when (third forms)
                        (let ((catch-forms (types:mal-data-value (third forms))))
                          (when (types:mal-data-value= mal-catch*
                                            (first catch-forms))
                            (return (mal-eval (third catch-forms)
                                              (env:create-mal-env :parent env
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
                                     env (env:create-mal-env :parent (cdr (assoc 'env attrs))
                                                             :binds (map 'list
                                                                         #'identity
                                                                         (types:mal-data-value (cdr (assoc 'params attrs))))
                                                             :exprs (cdr evaluated-list)))))
                            ((types:mal-builtin-fn-p function)
                             (return (apply (types:mal-data-value function)
                                            (cdr evaluated-list))))
                            (t (error 'invalid-function
                                      :form function
                                      :context "apply")))))))))))

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

(env:set-env *repl-env*
             (types:make-mal-symbol "eval")
             (types:make-mal-builtin-fn (lambda (ast)
                                          (mal-eval ast *repl-env*))))

(env:set-env *repl-env*
             (types:make-mal-symbol "*cl-implementation*")
             (make-mal-string (lisp-implementation-type)))

(env:set-env *repl-env*
             (types:make-mal-symbol "*cl-version*")
             (make-mal-string (lisp-implementation-version)))

(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
(rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
(rep "(def! *host-language* \"common-lisp\")")
(rep "(def! *gensym-counter* (atom 0))")
(rep "(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))")
(rep "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))")

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
  (rep "(println (str \"Mal [\" *host-language* \"]\"))")
  (loop do (let ((line (mal-readline "user> ")))
             (if line
                 (mal-writeline (rep line))
                 (return)))))

(defun run-file (file)
  (rep (format nil "(load-file \"~a\")" file)))

(defun main (&optional (argv nil argv-provided-p))

  (setf *use-readline-p* (not (or (string= (utils:getenv "PERL_RL") "false")
                                  (string= (utils:getenv "TERM") "dumb"))))

  ;; In GNU CLISP's batch mode the standard-input seems to be set to some sort
  ;; of input string-stream, this interacts wierdly with the PERL_RL enviroment
  ;; variable which the test runner sets causing `read-line' on *standard-input*
  ;; to fail with an empty stream error. The following reinitializes the
  ;; standard streams
  ;;
  ;; See http://www.gnu.org/software/clisp/impnotes/streams-interactive.html
  #+clisp (setf *standard-input* (ext:make-stream :input)
                *standard-output* (ext:make-stream :output :buffered t)
                *error-output* (ext:make-stream :error :buffered t))

  (let ((args (if argv-provided-p
                  argv
                  (cdr (utils:raw-command-line-arguments)))))
    (env:set-env *repl-env*
                 (types:make-mal-symbol "*ARGV*")
                 (make-mal-list (mapcar #'make-mal-string (cdr args))))
    (if (null args)
        (repl)
        (run-file (car args)))))

;;; Workaround for CMUCL's printing of "Reloaded library ... " messages when an
;;; image containing foreign libraries is restored. The extra messages cause the
;;; MAL testcases to fail

#+cmucl (progn
          (defvar *old-standard-output* *standard-output*
            "Keep track of current value standard output, this is restored after image restore completes")

          (defun muffle-output ()
            (setf *standard-output* (make-broadcast-stream)))

          (defun restore-output ()
            (setf *standard-output* *old-standard-output*))

          (pushnew #'muffle-output ext:*after-save-initializations*)
          (setf ext:*after-save-initializations*
                (append ext:*after-save-initializations* (list #'restore-output))))
