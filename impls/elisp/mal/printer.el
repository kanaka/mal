(require 'cl-lib)

(defun pr-str (form &optional print-readably)
  (let ((value (mal-value form)))
    (cl-ecase (mal-type form)
     ('nil
      "nil")
     (true
      "true")
     (false
      "false")
     (number
      (number-to-string value))
     (string
      (if print-readably
          (let ((print-escape-newlines t))
            (prin1-to-string value))
        value))
     ((symbol keyword)
      (symbol-name value))
     (list
      (pr-list value print-readably))
     (vector
      (pr-vector value print-readably))
     (map
      (pr-map value print-readably))
     (fn
      "#<fn>")
     (func
      "#<func>")
     (atom
      (format "(atom %s)" (pr-str value print-readably))))))

(defun pr-list (form print-readably)
  (let ((items (mapconcat
                (lambda (item) (pr-str item print-readably))
                form " ")))
    (concat "(" items ")")))

(defun pr-vector (form print-readably)
  (let ((items (mapconcat
                (lambda (item) (pr-str item print-readably))
                (append form nil) " ")))
    (concat "[" items "]")))

(defun pr-map (form print-readably)
  (let (pairs)
    (maphash
     (lambda (key value)
       (push (cons (pr-str key print-readably)
                   (pr-str value print-readably))
             pairs))
     form)
    (let ((items (mapconcat
                  (lambda (item) (concat (car item) " " (cdr item)))
                  (nreverse pairs) " ")))
      (concat "{" items "}"))))

(provide 'mal/printer)
