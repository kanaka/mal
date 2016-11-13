(defpackage :reader
  (:use :common-lisp :regexp :utils :types)
  (:export :read-str
           :eof))

(in-package :reader)

(defvar *string-re* (regexp:regexp-compile "^\"\\(\\\\\\(.\\|
\\)\\|[^\"\\]\\)*\"$")
  "Regular expression to match string")

(defvar *digit-re* (regexp:regexp-compile "^\\(-\\|+\\)\\?[[:digit:]]\\+$")
  "Regular expression to match digits")

(defvar *tokenizer-re* (regexp:regexp-compile "[[:space:],]*\\(~@\\|[][{}()~`'^@]\\|\"\\(\\\\\\(.\\|
\\)\\|[^\"\\]\\)*\"\\?\\|;[^
]*\\|[^][[:space:]~{}()@^`'\";]*\\)")
  "Regular expression to match LISP code")

(define-condition eof (types:mal-error)
  ((context :initarg :context :reader context))
  (:report (lambda (condition stream)
             (format stream
                     "EOF encountered while reading ~a"
                     (context condition)))))

(defun parse-string (token)
  (if (and (> (length token) 1)
           (regexp:regexp-exec *string-re* token))
      (progn
        (read-from-string (utils:replace-all token
                                             "\\n"
                                             "
")))
      ;; A bit inaccurate
      (error 'eof
             :context "string")))

;; Useful to debug regexps
(defun test-re (re string)
  (let ((match (regexp:match re string)))
    (when match
      (regexp:match-string string match))))

(defun test-tokenizer (re string)
  (let ((*tokenizer-re* re))
    (tokenize string)))

(defun tokenize (string)
  (let (tokens)
    (do* ((start 0)
          (end (length string))
          (match t))
         ((not match))
      (setf match (when (< start end)
                    (nth-value 1
                               (regexp:regexp-exec *tokenizer-re* string :start start))))
      (when match
        (setf start (regexp:match-end match))
        (let ((token (string-trim "," (regexp:match-string string match))))
          (unless (or (zerop (length token))
                      (char= (char token 0) #\;))
            (push token tokens)))))
    (nreverse tokens)))

(defstruct (token-reader)
  (tokens nil))

(defun peek (reader)
  (car (token-reader-tokens reader)))

(defun next (reader)
  (pop (token-reader-tokens reader)))

(defun consume (reader)
  (pop (token-reader-tokens reader))
  reader)

(defun read-str (string)
  (read-form (make-token-reader :tokens (tokenize string))))

(defun read-form (reader)
  (let ((token (peek reader)))
    (cond
      ((null token) nil)
      ((string= token "(") (make-mal-list (read-mal-sequence reader
                                                             ")"
                                                             'list)))
      ((string= token "[") (make-mal-vector (read-mal-sequence reader
                                                               "]"
                                                               'vector)))
      ((string= token "{") (make-mal-hash-map (read-hash-map reader)))
      ((string= token "^") (read-form-with-meta reader))
      ((member token '("'" "`" "~" "~@" "@") :test #'string= ) (expand-quote reader))
      (t (read-atom reader)))))

(defun read-form-with-meta (reader)
  (consume reader)
  (let ((meta (read-form reader))
        (value (read-form reader)))

    (when (or (null meta)
              (null value))
      (error 'eof
             :context "object metadata"))

    (make-mal-list (list (make-mal-symbol "with-meta") value meta))))

(defun expand-quote (reader)
  (let ((quote (next reader)))
    (make-mal-list (list (make-mal-symbol (cond
                                            ((string= quote "'") "quote")
                                            ((string= quote "`") "quasiquote")
                                            ((string= quote "~") "unquote")
                                            ((string= quote "~@") "splice-unquote")
                                            ((string= quote "@") "deref")))
                         (read-form reader)))))

(defun read-mal-sequence (reader &optional (delimiter ")") (constructor 'list))
  ;; Consume the opening brace
  (consume reader)
  (let (forms)
    (loop
       for token = (peek reader)
       while (cond
               ((null token) (error 'eof
                                    :context (if (string= delimiter ")")
                                                 "list"
                                                 "vector")))
               ((string= token delimiter) (return))
               (t (push (read-form reader) forms))))
    ;; Consume the closing brace
    (consume reader)
    (apply constructor (nreverse forms))))

(defun read-hash-map (reader)
  ;; Consume the open brace
  (consume reader)
  (let (forms)
    (loop
       for token = (peek reader)
       while (cond
               ((null token) (error 'eof
                                    :context "hash-map"))
               ((string= token "}") (return))
               (t (let ((key (read-form reader))
                        (value (read-form reader)))
                    (if (null value)
                        (error 'eof
                               :context "hash-map")
                        (push (cons key value) forms))))))
    ;; Consume the closing brace
    (consume reader)
    (make-hash-table :test 'types:mal-value=
                     :initial-contents (nreverse forms))))

(defun read-atom (reader)
  (let ((token (next reader)))
    (cond
      ((string= token "false")
       types:mal-false)
      ((string= token "true")
       types:mal-true)
      ((string= token "nil")
       types:mal-nil)
      ((char= (char token 0) #\")
       (make-mal-string (parse-string token)))
      ((char= (char token 0) #\:)
       (make-mal-keyword token))
      ((regexp:regexp-exec *digit-re* token)
       (make-mal-number (read-from-string token)))
      (t (make-mal-symbol token)))))
