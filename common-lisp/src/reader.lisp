(defpackage :reader
  (:use :common-lisp
        :types
        :alexandria)
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
                     "EOF encountered while reading '~a'"
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
      (error 'unexpected-token :expected token :actual actual-token)))
  reader)

(defun parse-string (token)
  ;; read-from-string doesn't handle \n
  (if (and (> (length token) 1)
           (scan *string-re* token))
      (let ((input (subseq token 1 (1- (length token)))))
        (with-output-to-string (out)
          (with-input-from-string (in input)
            (loop while (peek-char nil in nil)
                  do (let ((char (read-char in)))
                       (if (eql char #\\ )
                           (let ((char (read-char in)))
                             (if (eql char #\n)
                                 (terpri out)
                               (princ char out)))
                         (princ char out)))))))
      (error 'eof :context "string")))

(defun expand-quote (reader)
  (let ((quote-sym (make-mal-symbol (switch ((next reader) :test #'string=)
                                      ("'" "quote")
                                      ("`" "quasiquote")
                                      ("~" "unquote")
                                      ("~@" "splice-unquote")
                                      ("@" "deref")))))
    (make-mal-list (list quote-sym (read-form reader)))))

(defun read-mal-sequence (reader &optional (type 'list) &aux forms)
  (let ((context (string-downcase (symbol-name type)))
        (delimiter (if (equal type 'list) ")" "]")))

    ;; Consume the opening brace
    (consume reader)

    (setf forms (loop
                   until (string= (peek reader) delimiter)
                   collect (read-form-or-eof reader context)))

    ;; Consume the closing brace
    (consume reader)

    (apply type forms)))

(defun read-hash-map (reader)
  (let ((map (make-mal-value-hash-table))
        (context "hash-map"))

    ;; Consume the open brace
    (consume reader)

    (loop
       until (string= (peek reader) "}")
       do (setf (hashref (read-form-or-eof reader context) map)
                (read-form-or-eof reader context)))

    ;; Consume the closing brace
    (consume reader)

    map))

(defun read-atom (reader)
  (let ((token (next reader)))
    (cond ((string= token "false") mal-false)
          ((string= token "true") mal-true)
          ((string= token "nil") mal-nil)
          ((char= (char token 0) #\") (make-mal-string (parse-string token)))
          ((char= (char token 0) #\:) (make-mal-keyword token))
          ((scan *number-re* token) (make-mal-number (read-from-string token)))
          (t (make-mal-symbol token)))))

(defun read-form-with-meta (reader)
  (consume reader)

  (let ((meta (read-form-or-eof reader "object meta"))
        (value (read-form-or-eof reader "object meta")))
    (make-mal-list (list (make-mal-symbol "with-meta") value meta))))

(defun read-form (reader)
  (switch ((peek reader) :test #'equal)
    (nil nil)
    ("(" (make-mal-list (read-mal-sequence reader 'list)))
    ("[" (make-mal-vector (read-mal-sequence reader 'vector)))
    ("{" (make-mal-hash-map (read-hash-map reader)))
    ("^" (read-form-with-meta reader))
    ("'" (expand-quote reader))
    ("`" (expand-quote reader))
    ("~" (expand-quote reader))
    ("~@" (expand-quote reader))
    ("@" (expand-quote reader))
    (t (read-atom reader))))

(defun read-form-or-eof (reader context)
  (or (read-form reader)
      (error 'eof :context context)))

(defun read-str (string)
  (read-form (make-token-reader :tokens (tokenize string))))
