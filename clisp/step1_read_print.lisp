(require "dependencies")

(defpackage :mal
  (:use :common-lisp
        :readline
        :reader
        :printer))

(in-package :mal)

(defun mal-read (string)
  (reader:read-str string))

(defun mal-eval (ast env)
  ast)

(defun mal-print (expression)
  (printer:pr-str expression))

(defun rep (string)
  (handler-case
      (mal-print (mal-eval (mal-read string)
                           nil))
    (reader:eof (condition)
      (format nil
              "~a"
              condition))))

;; Readline setup
;;; The test runner sets this environment variable, in which case we do
;;; use readline since tests do not work with the readline interface
(defvar use-readline-p (not (string= (ext:getenv "PERL_RL") "false")))

(defvar *history-file* (file-namestring (merge-pathnames (user-homedir-pathname)
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

(main)
