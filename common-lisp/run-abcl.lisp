(require 'asdf)
(push *default-pathname-defaults* asdf:*central-registry*)

;; Suppress compilation output
(let ((*error-output* (make-broadcast-stream))
      (*standard-output* (make-broadcast-stream)))
  (asdf:load-system (car ext:*command-line-argument-list*) :verbose nil))

(mal:main (cdr ext:*command-line-argument-list*))
(cl-user::quit)
