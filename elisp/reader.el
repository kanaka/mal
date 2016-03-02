(defvar tokens nil)

(defun peek ()
  (car tokens))

(defun next ()
  (pop tokens))

(defun read-str (input)
  (setq tokens (tokenizer input))
  (read-form))

(defun tokenizer (input)
  (let (output)
    (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at token-re)
          (let ((token (match-string 1)))
            (if (= (length token) 0)
                (let ((remainder (buffer-substring (point) (point-max))))
                  (push remainder output)
                  (goto-char (point-max)))
              (when (not (string-match-p comment-re token))
                (push token output))
              (goto-char (match-end 1))))))
      (nreverse output))))

(defun read-form ()
  (let ((token (peek)))
    (cond
     ((string= token "'")
      (read-quote))
     ((string= token "`")
      (read-quasiquote))
     ((string= token "~")
      (read-unquote))
     ((string= token "~@")
      (read-splice-unquote))
     ((string= token "@")
      (read-deref))
     ((string= token "^")
      (read-with-meta))
     ((string= token "(")
      (read-list))
     ((string= token "[")
      (read-vector))
     ((string= token "{")
      (read-map))
     (t
      ;; assume anything else is an atom
      (read-atom)))))

(defun read-simple-reader-macro (symbol)
  (next) ; pop reader macro token
  ;; turn form into (symbol form)
  (mal-list (list (mal-symbol symbol) (read-form))))

(defun read-quote ()
  (read-simple-reader-macro 'quote))

(defun read-quasiquote ()
  (read-simple-reader-macro 'quasiquote))

(defun read-unquote ()
  (read-simple-reader-macro 'unquote))

(defun read-splice-unquote ()
  (read-simple-reader-macro 'splice-unquote))

(defun read-deref ()
  (read-simple-reader-macro 'deref))

(defun read-with-meta ()
  (next) ; pop with-meta token
  (let ((meta (read-form)))
    (mal-list (list (mal-symbol 'with-meta) (read-form) meta))))

(defun read-list ()
  (next) ; pop list start
  (let (output end-of-list)
    (while (not end-of-list)
      (let ((token (peek)))
        (cond
         ((string= token ")")
          (next) ; pop list end
          (setq end-of-list t))
         ((not token)
          (signal 'unterminated-sequence '(list)))
         (t
          (push (read-form) output)))))
    (mal-list (nreverse output))))

(defun read-vector ()
  (next) ; pop vector start
  (let (output end-of-vector)
    (while (not end-of-vector)
      (let ((token (peek)))
        (cond
         ((string= token "]")
          (next) ; pop vector end
          (setq end-of-vector t))
         ((not token)
          (signal 'unterminated-sequence '(vector)))
         (t
          (push (read-form) output)))))
    (mal-vector (vconcat (nreverse output)))))

;; HACK overriden by core.el in later steps
(define-hash-table-test 'mal-= 'equal 'sxhash)

(defun read-map ()
  (next) ; pop map start
  (let ((output (make-hash-table :test 'mal-=))
        end-of-map)
    (while (not end-of-map)
      (let ((token (peek)))
        (cond
         ((string= token "}")
          (next) ; pop map end
          (setq end-of-map t))
         ((not token)
          (signal 'unterminated-sequence '(map)))
         (t
          (puthash (read-form) (read-form) output)))))
    (mal-map output)))

(defun read-atom ()
  (let ((token (next)))
    (if token
        (cond
         ((string= token "nil")
          mal-nil)
         ((string= token "true")
          mal-true)
         ((string= token "false")
          mal-false)
         ((string-match number-re token)
          (mal-number (string-to-number token)))
         ((= (aref token 0) ?\")
          (if (string-match string-re token)
              (mal-string (read token))
            (signal 'unterminated-sequence '(string))))
         ((= (aref token 0) ?:)
          (mal-keyword (intern token)))
         (t
          ;; assume anything else is a symbol
          (mal-symbol (intern token))))
      (signal 'end-of-token-stream nil))))
