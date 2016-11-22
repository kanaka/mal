#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :uiop :silent t :verbose nil)
(ql:quickload :cl-readline :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :genhash :silent t)

(defpackage #:mal-asd
  (:use :cl :asdf))

(in-package :mal-asd)

(defsystem "stepA_mal"
  :name "MAL"
  :version "1.0"
  :author "Iqbal Ansari"
  :description "Implementation of MAL in Common Lisp"
  :serial t
  :components ((:file "utils")
               (:file "types")
               (:file "env")
               (:file "reader")
               (:file "printer")
               (:file "core")
               (:file "stepA_mal"))
  :depends-on (:uiop :cl-readline :cl-ppcre :genhash)
  :pathname "src/")
