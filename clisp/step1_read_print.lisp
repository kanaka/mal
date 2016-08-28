(require "dependencies")

(defpackage :mal
  (:use :common-lisp :reader :printer))

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
