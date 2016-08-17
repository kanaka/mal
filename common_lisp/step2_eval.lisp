(require "reader")
(require "printer")
(require "types")
(require "env")

(defpackage :mal
  (:use :common-lisp :types :env :reader :printer))

(in-package :mal)

;; Environment

(defvar *repl-env* (make-hash-table :test 'types:mal-value=))

(setf (gethash (types:make-mal-symbol '+) *repl-env*)
      (types:make-mal-builtin-fn (lambda (value1 value2)
                                   (apply-unwrapped-values '+
                                                           value1
                                                           value2))))

(setf (gethash (types:make-mal-symbol '-) *repl-env*)
      (types:make-mal-builtin-fn (lambda (value1 value2)
                                   (apply-unwrapped-values '-
                                                           value1
                                                           value2))))

(setf (gethash (types:make-mal-symbol '*) *repl-env*)
      (types:make-mal-builtin-fn (lambda (value1 value2)
                                   (apply-unwrapped-values '*
                                                           value1
                                                           value2))))

(setf (gethash (types:make-mal-symbol '/) *repl-env*)
      (types:make-mal-builtin-fn (lambda (value1 value2)
                                   (apply-unwrapped-values '/
                                                           value1
                                                           value2))))

(defun lookup-env (symbol env)
  (let ((value (gethash symbol env)))
    (if value
        value
        (error 'env:undefined-symbol
               :symbol (format nil "~a" (types:mal-value symbol))))))

(defun mal-read (string)
  (reader:read-str string))

(defun mal-eval (ast env)
  (cond
    ((not (types:mal-list-p ast)) (eval-ast ast env))
    ((zerop (length (mal-value ast))) ast)
    (t (progn
         (let ((evaluated-list (eval-ast ast env)))
           (apply (mal-value (car evaluated-list))
                  (cdr evaluated-list)))))))

(defun mal-print (expression)
  (printer:pr-str expression))

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
    (types:symbol (lookup-env ast env))
    (types:list (eval-sequence ast env))
    (types:vector (make-mal-vector (apply 'vector (eval-sequence ast env))))
    (types:hash-map (eval-hash-map ast env ))
    (types:any ast)))

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
