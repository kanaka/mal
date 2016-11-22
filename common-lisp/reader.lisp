(defpackage :reader
  (:use :common-lisp
        :types)
  (:import-from :genhash
                :hashref)
  (:import-from :cl-ppcre
                :create-scanner
                :do-matches-as-strings
                :scan)
  (:import-from :utils
                :replace-all)
  (:export :read-str
           :eof
           :unexpected-token))

(in-package :reader)

;; Possible errors that can be raised while reading a string
(define-condition unexpected-token (error)
  ((expected :initarg :expected :reader expected-token)
   (actual :initarg :actual :reader actual-token))
  (:report (lambda (condition stream)
             (format stream
                     "Unexpected token (~a) encountered while reading, expected ~a"
                     (actual-token condition)
                     (expected-token condition))))
  (:documentation "Error raised when an unexpected token is encountered while reading."))

(define-condition eof (error)
  ((context :initarg :context :reader context))
  (:report (lambda (condition stream)
             (format stream
                     "EOF encountered while reading ~a"
                     (context condition))))
  (:documentation "Error raised when EOF is encountered while reading."))

(defvar *tokenizer-re* (create-scanner "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)")
  "Regular expression to tokenize Lisp code")

(defvar *number-re* (create-scanner "^(-|\\+)?[\\d]+$")
  "Regular expression to match a number")

(defvar *string-re* (create-scanner "^\"(?:\\\\.|[^\\\\\"])*\"$")
  "Regular expression to match a string")

(defvar *whitespace-chars*
  '(#\Space #\Newline #\Backspace #\Tab
    #\Linefeed #\Page #\Return #\Rubout #\,)
  "Characters to treat as whitespace, these are trimmed in `tokenize'")

(defun tokenize (string)
  "Tokenize given string.

This function extracts all tokens from the string using *tokenizer-re*
comments are ignored.

Implementation notes: The regex scan generates some empty tokens, not really
sure why."
  (let (tokens)
    (do-matches-as-strings (match *tokenizer-re* string)
      (let ((token (string-trim *whitespace-chars* match)))
        (unless (or (zerop (length token))
                    (char= (char token 0) #\;))
          (push token tokens))))
    (nreverse tokens)))

;; Reader
(defstruct (token-reader)
  (tokens nil))

(defun peek (reader)
  "Returns the next token in the reader without advancing the token stream."
  (car (token-reader-tokens reader)))

(defun next (reader)
  "Returns the next token and advances the token stream."
  (pop (token-reader-tokens reader)))

(defun consume (reader &optional (token nil token-provided-p))
  "Consume the next token and advance the token stream.

If the optional argument token is provided the token stream is advanced only
if token being consumes matches it otherwise and unexpected token error is
raised"
  (let ((actual-token (pop (token-reader-tokens reader))))
    (when (and token-provided-p
               (not (equal actual-token token)))
      (error 'unexpected-token
             :expected token
             :actual actual-token)))
  reader)

(defun parse-string (token)
  (if (and (> (length token) 1)
           (scan *string-re* token))
      (read-from-string (utils:replace-all token
                                             "\\n"
                                             "
"))
      (error 'eof
             :context "string")))

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
  (let (forms
        (hash-map (types:make-mal-value-hash-table)))
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
    ;; Construct the hash table
    (dolist (key-value (nreverse forms))
      (setf (genhash:hashref (car key-value) hash-map) (cdr key-value)))
    hash-map))

(defun read-atom (reader)
  (let ((token (next reader)))
    (cond
      ((string= token "false")
       mal-false)
      ((string= token "true")
       mal-true)
      ((string= token "nil")
       mal-nil)
      ((char= (char token 0) #\")
       (make-mal-string (parse-string token)))
      ((char= (char token 0) #\:)
       (make-mal-keyword token))
      ((scan *number-re* token)
       (make-mal-number (read-from-string token)))
      (t (make-mal-symbol token)))))

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
      ((member token '("'" "`" "~" "~@" "@") :test #'string=) (expand-quote reader))
      (t (read-atom reader)))))

(defun read-str (string)
  (read-form (make-token-reader :tokens (tokenize string))))
