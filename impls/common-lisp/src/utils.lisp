(defpackage :utils
  (:use :common-lisp
        :uiop)
  (:export :replace-all
           :getenv
           :read-file-string
           :raw-command-line-arguments
           :listify
           :common-prefix))

(in-package :utils)

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defun listify (sequence)
  "Convert a sequence to a list"
  (map 'list #'identity sequence))

(defun common-prefix (&rest strings)
  (if (not strings)
      ""
      (let* ((char-lists (mapcar (lambda (string) (coerce string 'list)) strings))
             (char-tuples (apply #'mapcar #'list char-lists))
             (count 0))
        (loop for char-tuple in char-tuples
           while (every (lambda (char) (equal char (car char-tuple))) char-tuple)
           do (incf count))

        (subseq (car strings) 0 count))))
