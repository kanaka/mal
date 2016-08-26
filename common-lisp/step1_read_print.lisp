(defpackage :mal
  (:use :common-lisp
        :reader
        :printer
        :utils)
  (:export :main))

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
                           (make-hash-table :test #'equal)))
    (reader:eof (condition)
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
