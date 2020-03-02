(define-library (lib reader)

(export read-str)

(import (scheme base))
(import (scheme char))
(import (scheme read))
(import (scheme write))

(import (lib util))
(import (lib types))

(begin

(define-record-type reader
  (%make-reader tokens position)
  reader?
  (tokens %reader-tokens)
  (position %reader-position %reader-position-set!))

(define (make-reader tokens)
  (%make-reader (list->vector tokens) 0))

(define (peek reader)
  (let ((tokens (%reader-tokens reader))
        (position (%reader-position reader)))
    (if (>= position (vector-length tokens))
        #f
        (vector-ref tokens position))))

(define (next reader)
  (let ((token (peek reader)))
    (when token
      (%reader-position-set! reader (+ (%reader-position reader) 1)))
    token))

(define (read-str input)
  (let* ((tokens (tokenizer input))
         (reader (make-reader tokens)))
    (read-form reader)))

(define (whitespace-char? char)
  (or (char-whitespace? char) (char=? char #\,)))

(define (special-char? char)
  (memv char '(#\[ #\] #\{ #\} #\( #\) #\' #\` #\~ #\^ #\@)))

(define (non-word-char? char)
  (or (whitespace-char? char)
      (memv char '(#\[ #\] #\{ #\} #\( #\) #\' #\" #\` #\;))))

(define (tokenizer input)
  (call-with-input-string input
    (lambda (port)
      (let loop ((tokens '()))
        (if (eof-object? (peek-char port))
            (reverse tokens)
            (let ((char (read-char port)))
              (cond
               ((whitespace-char? char)
                (loop tokens))
               ((and (char=? char #\~)
                     (char=? (peek-char port) #\@))
                (read-char port) ; remove @ token
                (loop (cons "~@" tokens)))
               ((char=? char #\")
                (loop (cons (tokenize-string port) tokens)))
               ((char=? char #\;)
                (skip-comment port)
                (loop tokens))
               ((special-char? char)
                (loop (cons (char->string char) tokens)))
               (else
                (loop (cons (tokenize-word port char) tokens))))))))))

(define (tokenize-string port)
  (let loop ((chars '(#\")))
    (let ((char (read-char port)))
      (cond
       ((eof-object? char)
        (list->string (reverse chars)))
       ((char=? char #\\)
        (let ((char (read-char port)))
          (when (not (eof-object? char))
            (loop (cons char (cons #\\ chars))))))
       ((not (char=? char #\"))
        (loop (cons char chars)))
       ((char=? char #\")
        (list->string (reverse (cons #\" chars))))))))

(define (skip-comment port)
  (let loop ()
    (let ((char (peek-char port)))
      (when (not (or (eof-object? char)
                     (char=? char #\newline)))
        (read-char port)
        (loop)))))

(define (tokenize-word port char)
  (let loop ((chars (list char)))
    (let ((char (peek-char port)))
      (if (or (eof-object? char)
              (non-word-char? char))
          (list->string (reverse chars))
          (loop (cons (read-char port) chars))))))

(define (read-form reader)
  (let ((token (peek reader)))
  (cond
   ((equal? token "'")
    (read-macro reader 'quote))
   ((equal? token "`")
    (read-macro reader 'quasiquote))
   ((equal? token "~")
    (read-macro reader 'unquote))
   ((equal? token "~@")
    (read-macro reader 'splice-unquote))
   ((equal? token "@")
    (read-macro reader 'deref))
   ((equal? token "^")
    (read-meta reader))
   ((equal? token "(")
    (read-list reader ")" mal-list))
   ((equal? token "[")
    (read-list reader "]" (lambda (items) (mal-vector (list->vector items)))))
   ((equal? token "{")
    (read-list reader "}" (lambda (items) (mal-map (list->alist items)))))
   (else
    (read-atom reader)))))

(define (read-macro reader symbol)
  (next reader) ; pop macro token
  (mal-list (list (mal-symbol symbol) (read-form reader))))

(define (read-meta reader)
  (next reader) ; pop macro token
  (let ((form (read-form reader)))
    (mal-list (list (mal-symbol 'with-meta) (read-form reader) form))))

(define (read-list reader ender proc)
  (next reader) ; pop list start
  (let loop ((items '()))
    (let ((token (peek reader)))
      (cond
       ((equal? token ender)
        (next reader)
        (proc (reverse items)))
       ((not token)
        (error (str "expected '" ender "', got EOF")))
       (else
        (loop (cons (read-form reader) items)))))))

(define (read-atom reader)
  (let ((token (next reader)))
    (cond
     ((not token)
      (error "end of token stream" 'empty-input))
     ((equal? token "true")
      mal-true)
     ((equal? token "false")
      mal-false)
     ((equal? token "nil")
      mal-nil)
     ((string->number token)
      => mal-number)
     ((char=? (string-ref token 0) #\")
      (guard
       (ex ((cond-expand
             ;; HACK: https://github.com/ashinn/chibi-scheme/pull/540
             (chibi
              (error-object? ex))
             (else
              (read-error? ex)))
            (error (str "expected '" #\" "', got EOF"))))
       (mal-string (call-with-input-string token read))))
     ((char=? (string-ref token 0) #\:)
      (mal-keyword (string->symbol (string-copy token 1))))
     (else
      (mal-symbol (string->symbol token))))))

)

)
