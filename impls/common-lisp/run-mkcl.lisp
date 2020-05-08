(require 'asdf)
(push *default-pathname-defaults* asdf:*central-registry*)

(defvar *raw-command-line-args* (loop
                                   :for index
                                   :from 1
                                   :below (mkcl:argc)
                                   :collect (mkcl:argv index)))

(defvar *command-line-args* (subseq *raw-command-line-args*
                                    (min (1+ (position "--" *raw-command-line-args* :test #'string=))
                                         (length *raw-command-line-args*))))

;; Suppress compilation output
(let ((*error-output* (make-broadcast-stream))
      (*standard-output* (make-broadcast-stream)))
  (format *standard-output* "~a" *command-line-args*)
  (asdf:load-system (car *command-line-args*) :verbose nil))

(mal:main (cdr *command-line-args*))
(quit)
