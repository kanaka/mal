;;; general accessors

(defun mal-type (mal-object)
  (aref mal-object 0))

(defun mal-value (mal-object)
  (aref mal-object 1))

(defun mal-meta (mal-object)
  (aref mal-object 2))

;;; objects

(defmacro mal-object (name)
  (let ((constructor (intern (format "mal-%s" name)))
        (predicate (intern (format "mal-%s-p" name))))
    `(progn
       (defun ,constructor (&optional value meta)
         (vector ',name value meta))
       (defun ,predicate (arg)
         (and (arrayp arg) (eq (aref arg 0) ',name))))))

(mal-object nil)
(mal-object true)
(mal-object false)

(defvar mal-nil (mal-nil))
(defvar mal-true (mal-true 'true))
(defvar mal-false (mal-false 'false))

(mal-object number)
(mal-object string)
(mal-object symbol)
(mal-object keyword)

(mal-object list)
(mal-object vector)
(mal-object map)

(mal-object env)
(mal-object atom)

(mal-object fn)
(mal-object func)

;;; regex

(defvar token-re
  (rx (* (any white ?,))                     ;; leading whitespace
      (group
       (or
        "~@"                                 ;; special 2-char token
        (any "[]{}()'`~^@")                  ;; special 1-char tokens
        (and ?\" (* (or (and ?\\ anything)
                        (not (any "\\\""))))
             ?\")                            ;; string with escapes
        (and ?\; (* not-newline))            ;; comment
        (* (not (any white "[]{}()'\"`,;"))) ;; catch-all
        ))))

(defvar whitespace-re
  (rx bos (* (any white ?,)) eos))

(defvar comment-re
  (rx bos ?\; (* anything)))

(defvar sequence-end-re
  (rx bos (any ")]}") eos))

(defvar number-re
  (rx bos (? (any "+-")) (+ (char digit)) eos))

(defvar string-re
  (rx bos ?\" (* (or (and ?\\ anything)
                     (not (any "\\\""))))
      ?\" eos))

;;; errors

(when (not (fboundp 'define-error))
  (defun define-error (name message &optional parent)
  "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
  (unless parent (setq parent 'error))
  (let ((conditions
         (if (consp parent)
             (apply #'nconc
                    (mapcar (lambda (parent)
                              (cons parent
                                    (or (get parent 'error-conditions)
                                        (error "Unknown signal `%s'" parent))))
                            parent))
           (cons parent (get parent 'error-conditions)))))
    (put name 'error-conditions
         (delete-dups (copy-sequence (cons name conditions))))
    (when message (put name 'error-message message)))))

(define-error 'mal "MAL error")
(define-error 'unterminated-sequence "Unterminated token sequence" 'mal)
(define-error 'end-of-token-stream "End of token stream" 'mal)
(define-error 'mal-custom "Custom error" 'mal)
