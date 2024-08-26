;; Structural pattern matching is ideal, but too slow for MAL.

;; So we use a mal-foo-value getter that returns nil in case of bad
;; type (or if a list is empty, unfortunately).

(defmacro mal-object (name)
  (let ((constructor (intern (format "mal-%s" name)))
        (accessor (intern (format "mal-%s-value" name))))
    `(progn
       (defsubst ,constructor (value)
         (record ',name value))
       (defun ,accessor (arg)
         (and (recordp arg)
              (eq (aref arg 0) ',name)
              (aref arg 1))))))

(defconst mal-nil   #&8"n")
(defconst mal-false #&8"f")
(defconst mal-true  #&8"t")

(defsubst mal-number (elisp-number) elisp-number)
(defsubst mal-number-value (obj) (and (numberp obj) obj))

(defsubst mal-symbol (elisp-symbol) elisp-symbol)
;; A nil result means either 'not a symbol' or 'the nil symbol'.
(defsubst mal-symbol-value (obj) (and (symbolp obj)obj))

(defsubst mal-string (elisp-string) elisp-string)
(defsubst mal-string-value (obj) (and (stringp obj) obj))

;; In elisp, keywords are symbols.  Using them would cause confusion,
;; or at least make mal-symbol-value more complex, for little benefit.
;; The wrapped value is an elisp string including the initial colon.
(mal-object keyword)

;; Use the native type when possible, but #s(type value meta ...) for
;; the empty list or when metadata is present.

(defsubst mal-vector (elisp-vector) elisp-vector)
(defun mal-vector-value (obj)
  (if (vectorp obj)
      obj
    (and (recordp obj) (eq (aref obj 0) 'vector) (aref obj 1))))

(defsubst mal-map (elisp-hash-table) elisp-hash-table)
(defun mal-map-value (obj)
  (if (hash-table-p obj)
      obj
    (and (recordp obj) (eq (aref obj 0) 'map) (aref obj 1))))

(defconst mal-empty-list #s(list nil))
(defsubst mal-list (elisp-list) (or elisp-list mal-empty-list))
;; A nil result means either 'not a list' or 'empty list'.
(defun mal-list-value (obj)
  (if (listp obj) obj
    (and (recordp obj) (eq (aref obj 0) 'list) (aref obj 1))))
(defun mal-list-p (obj)
  (or (listp obj)
      (and (recordp obj) (eq (aref obj 0) 'list))))

;; A nil result means either 'not a list' or 'empty list'.
(defun mal-seq-value (arg) (or (mal-vector-value arg) (mal-list-value arg)))

(mal-object atom)
(defun mal-reset (atom value) (setf (aref atom 1) value))

(mal-object fn-core)
(mal-object macro)

;; Function created by fn*.
(defsubst mal-func (value body params env)
  (record 'func value body params env))
(defun mal-func-value ( obj)
  (and (recordp obj) (eq (aref obj 0) 'func) (aref obj 1)))
(defsubst mal-func-body   (obj) (aref obj 2))
(defsubst mal-func-params (obj) (aref obj 3))
(defsubst mal-func-env    (obj) (aref obj 4))

(defun with-meta (obj meta)
  (cond
   ((vectorp      obj)  (record 'vector obj meta))
   ((hash-table-p obj)  (record 'map    obj meta))
   ((listp        obj)  (record 'list   obj meta))
   ((< (length obj) 4)  (record (aref obj 0) (aref obj 1) meta))
   (t                   (record (aref obj 0) (aref obj 1)
                                (aref obj 2) (aref obj 3)
                                (aref obj 4) meta))))

(defun mal-meta (obj)
  (if (and (recordp obj) (member (length obj) '(3 6)))
      (aref obj (1- (length obj)))
    mal-nil))

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
(define-error 'unterminated-sequence "Unexpected end of input during token sequence" 'mal)
(define-error 'end-of-token-stream "End of token stream" 'mal)
(define-error 'mal-custom "Custom error" 'mal)

(provide 'mal/types)
