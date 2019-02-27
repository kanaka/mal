;; For some reason MKCL fails to find libreadline.so as a result cl-readline
;; fails. To avoid conditionals in the code we fake the cl-readline interface
;; and use it in asdf definitions when running under MKCL
(defpackage   :cl-readline
  (:nicknames :rl)
  (:use :common-lisp))

(in-package :cl-readline)

(defun readline (&keys prompt already-prompted num-chars
                       erase-empty-line add-history novelty-check)
  (declare (ignorable ignored))
  (format *standard-output* prompt)
  (force-output *standard-output*)
  (read-line *standard-input* nil))

(defun register-function (&rest ignored)
  (declare (ignorable ignored)))
