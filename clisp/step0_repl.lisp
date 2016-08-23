(defpackage :mal
  (:use :common-lisp))

(in-package :mal)

(defun mal-read (string)
  string)

(defun mal-eval (ast env)
  ast)

(defun mal-print (expression)
  expression)

(defun rep (string)
  (mal-print (mal-eval (mal-read string)
                       (make-hash-table :test #'equal))))

(defun readline (prompt &optional (in-stream *standard-input*) (out-stream *standard-output*))
  (format out-stream prompt)
  (force-output out-stream)
  (read-line in-stream nil))

(defun main ()
  (loop do (let ((line (readline "user> ")))
             (if line (write-line (rep line)) (return)))))

(main)
