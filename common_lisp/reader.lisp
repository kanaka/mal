(require "types")

(defpackage :reader
  (:use :regexp :common-lisp :types)
  (:export :read-str))

(in-package :reader)

(defvar *two-char-token* "~@"
  "RE two char")

(defvar *single-char-token* "\\(\\[\\|\\]\\|[{}()`'^@]\\)"
  "RE single char")

(defvar *string-re* "\"\\(?:\\\\\\(?:.\\|\n\\)\\|[^\"\\]\\)*\""
  "RE string")

(defvar *comment-re* ";[^
]*"
  "RE comment")

(defvar *identifier-re* "[^[:space:]{}()`'\";]\\+"
  "RE identifier")

(defvar *tokenizer-re* "[[:space:],]*\\(~@\\|\\(\\[\\|\\]\\|[{}()`'^@]\\)\\|\"\\(\\\\\\(.\\|\n\\)\\|[^\"\\]\\)*\"\\|;[^
]*\\|[^[:space:]{}()`'\";]\\+\\)"
  "RE")

(define-condition eof (error)
  ((text :initarg :text)))

(defun test-re (re string)
  (let ((match (regexp:match re string)))
    (when match
      (regexp:match-string string match))))

(defvar *whitespace-chars*
  '(#\Space #\Newline #\Backspace #\Tab
    #\Linefeed #\Page #\Return #\Rubout #\,))

(defun tokenize (string)
  (remove-if (lambda (token)
               (or (zerop (length token))
                   (char= (char token 0) #\;)))
             (loop
                with end = (length string)
                for start = 0 then (regexp:match-end match)
                for match = (ignore-errors
                              (regexp:match *tokenizer-re* string :start start))
                while (and match (< start end))
                collect (string-trim *whitespace-chars*
                                     (regexp:match-string string match)))))

(defstruct (token-reader)
  (tokens nil))

(defun peek (reader)
  (car (token-reader-tokens reader)))

(defun next (reader)
  (pop (token-reader-tokens reader)))

(defun consume (reader)
  (pop (token-reader-tokens reader))
  reader)

(defun read-from-string-preserving-case (string)
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (read-from-string string)))

(defun read-str (string)
  (read-form (make-token-reader :tokens (tokenize string))))

(defun read-form (reader)
  (let ((token (peek reader)))
    (cond
      ((null token) nil)
      ((string= token "(") (read-list reader))
      (t (read-atom reader)))))

(defun read-list (reader)
  ;; Consume the open brace
  (consume reader)
  (let (forms)
    (loop
       for token = (peek reader)
       while (cond
               ((null token) (error 'eof :text "EOF encountered while reading list"))
               ((string= token ")") (return))
               (t (push (read-form reader) forms))))
    ;; Consume the closing brace
    (consume reader)
    (make-mal-list (nreverse forms))))

(defun read-atom (reader)
  (let ((token (next reader)))
    (cond
      ((regexp:match "^[[:digit:]]\\+$" token)
       (make-mal-number (read-from-string token)))
      ((string= token "false")
       (make-mal-boolean nil))
      ((string= token "true")
       (make-mal-boolean t))
      ((string= token "nil")
       (make-mal-nil nil))
      ((char= (char token 0) #\")
       (make-mal-string (read-from-string token)))
      (t (make-mal-symbol (read-from-string-preserving-case token))))))
