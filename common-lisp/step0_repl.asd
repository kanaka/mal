#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :uiop :silent t)
#-mkcl (ql:quickload :cl-readline :silent t)
#+mkcl (load "fake-readline.lisp")

(defpackage #:mal-asd
  (:use :cl :asdf))

(in-package :mal-asd)

(defsystem "step0_repl"
  :name "MAL"
  :version "1.0"
  :author "Iqbal Ansari"
  :description "Implementation of step 0 of MAL in Common Lisp"
  :serial t
  :components ((:file "step0_repl"))
  :depends-on (:uiop :cl-readline)
  :pathname "src/")
