(require "types")

(defpackage :reader
  (:use :regexp :common-lisp :types)
  (:export :read-str))

(in-package :reader)

(defvar *string-re* "^\"\\(\\\\\\(.\\|
\\)\\|[^\"\\]\\)*\"$"
  "RE string")

(defvar *tokenizer-re* "[[:space:],]*\\(~@\\|[][{}()`'^@]\\|\"\\(\\\\\\(.\\|
\\)\\|[^\"\\]\\)*\"\\?\\|;[^
]*\\|[^][[:space:]{}()`'\";]*\\)"
  "RE")

(define-condition eof (error)
  ((context :initarg :context :reader context))
  (:report (lambda (condition stream)
             (format stream
                     "EOF encountered while reading ~a"
                     (context condition)))))

(defun parse-string (token)
  (if (and (> (length token) 1)
           (regexp:match *string-re* token))
      (read-from-string token)
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
      ((string= token "(") (make-mal-list (read-mal-sequence reader
                                                             ")"
                                                             'list)))
      ((string= token "[") (make-mal-vector (read-mal-sequence reader
                                                               "]"
                                                               'vector)))
      ((string= token "{") (make-mal-hash-map (read-hash-map reader)))
      (t (read-atom reader)))))

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
        (hash-map (make-hash-table :test 'types:mal-value=)))
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
    (dolist (key-value forms)
      (setf (gethash (car key-value) hash-map) (cdr key-value)))
    hash-map))

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
       (make-mal-string (parse-string token)))
      (t (make-mal-symbol (read-from-string-preserving-case token))))))
