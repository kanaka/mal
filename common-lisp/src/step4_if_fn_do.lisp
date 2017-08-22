(defpackage :mal
  (:use :common-lisp
        :types
        :env
        :reader
        :printer
        :core)
  (:import-from :cl-readline
                :readline
                :register-function)
  (:import-from :genhash
                :hashref
                :hashmap)
  (:import-from :utils
                :listify
                :getenv
                :common-prefix)
  (:export :main))

(in-package :mal)

(defvar *repl-env* (env:create-mal-env))

(dolist (binding core:ns)
  (env:set-env *repl-env* (car binding) (cdr binding)))

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
  (let ((hash-map-value (mal-data-value hash-map))
        (new-hash-table (make-mal-value-hash-table)))
    (genhash:hashmap (lambda (key value)
                       (setf (genhash:hashref (mal-eval key env) new-hash-table)
                             (mal-eval value env)))
                     hash-map-value)
    (make-mal-hash-map new-hash-table)))

(defun eval-ast (ast env)
  (switch-mal-type ast
    (types:symbol (env:get-env env ast))
    (types:list (eval-sequence ast env))
    (types:vector (make-mal-vector (apply 'vector (eval-sequence ast env))))
    (types:hash-map (eval-hash-map ast env))
    (types:any ast)))

(defun eval-let* (forms env)
  (let ((new-env (env:create-mal-env :parent env))
        (bindings (utils:listify (mal-data-value (second forms)))))

    (mapcar (lambda (binding)
              (env:set-env new-env
                           (car binding)
                           (mal-eval (or (cdr binding)
                                         mal-nil)
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
      ((mal-data-value= mal-do (first forms))
       (car (last (mapcar (lambda (form) (mal-eval form env))
                          (cdr forms)))))
      ((mal-data-value= mal-if (first forms))
       (let ((predicate (mal-eval (second forms) env)))
         (mal-eval (if (or (mal-data-value= predicate mal-nil)
                           (mal-data-value= predicate mal-false))
                       (fourth forms)
                       (third forms))
                   env)))
      ((mal-data-value= mal-fn* (first forms))
       (make-mal-fn (let ((arglist (second forms))
                                (body (third forms)))
                            (lambda (&rest args)
                              (mal-eval body (env:create-mal-env :parent env
                                                                 :binds (listify (mal-data-value arglist))
                                                                 :exprs args))))))
      (t (let* ((evaluated-list (eval-ast ast env))
               (function (car evaluated-list)))
         ;; If first element is a mal function unwrap it
         (apply (mal-data-value function)
                (cdr evaluated-list)))))))

(defun mal-read (string)
  (reader:read-str string))

(defun mal-eval (ast env)
  (cond
    ((null ast) mal-nil)
    ((not (mal-list-p ast)) (eval-ast ast env))
    ((zerop (length (mal-data-value ast))) ast)
    (t (eval-list ast env))))

(defun mal-print (expression)
  (printer:pr-str expression))

(defun rep (string)
  (handler-case
      (mal-print (mal-eval (mal-read string) *repl-env*))
    (error (condition)
      (format nil "~a" condition))))

(rep "(def! not (fn* (a) (if a false true)))")

(defvar *use-readline-p* nil)

(defun complete-toplevel-symbols (input &rest ignored)
  (declare (ignorable ignored))

  (let (candidates)
    (loop for key being the hash-keys of (env:mal-env-bindings *repl-env*)
       when (let ((pos (search input key))) (and pos (zerop pos)))
       do (push key candidates))

    (if (= 1 (length candidates))
        (cons (car candidates) candidates)
        (cons (apply #'utils:common-prefix candidates) candidates))))

(defun raw-input (prompt)
  (format *standard-output* prompt)
  (force-output *standard-output*)
  (read-line *standard-input* nil))

(defun mal-readline (prompt)
  (if *use-readline-p*
      (rl:readline :prompt prompt :add-history t :novelty-check #'string/=)
      (raw-input prompt)))

(defun mal-writeline (string)
  (when string
    (write-line string)
    (force-output *standard-output*)))

(defun main (&optional (argv nil argv-provided-p))
  (declare (ignorable argv argv-provided-p))

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

  ;; CCL fails with a error while registering completion function
  ;; See also https://github.com/mrkkrp/cl-readline/issues/5
  #-ccl (rl:register-function :complete #'complete-toplevel-symbols)

  (loop do (let ((line (mal-readline "user> ")))
             (if line (mal-writeline (rep line)) (return)))))

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
