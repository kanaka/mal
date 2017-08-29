(defpackage :mal
  (:use :common-lisp)
  (:import-from :uiop
                :getenv)
  (:import-from :cl-readline
                :readline)
  (:export :main))

(in-package :mal)

(defun mal-read (string)
  string)

(defun mal-eval (ast)
  ast)

(defun mal-print (expression)
  expression)

(defun rep (string)
  (mal-print (mal-eval (mal-read string))))

(defvar *use-readline-p* nil)

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

  (setf *use-readline-p* (not (or (string= (uiop:getenv "PERL_RL") "false")
                                  (string= (uiop:getenv "TERM") "dumb"))))

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
