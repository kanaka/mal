#lang racket

(provide read_str)

(require "types.rkt")

(define Reader%
  (class object%
    (init tokens)
    (super-new)
    (define toks tokens)
    (define position 0)
    (define/public (next)
      (cond [(>= position (length toks)) null]
            [else (begin
                    (set! position (+ 1 position))
                    (list-ref toks (- position 1)))]))
    (define/public (peek)
      (cond [(>= position (length toks)) null]
            [else (list-ref toks position )]))))
      

(define (tokenize str)
  (filter-not (lambda (s) (or (equal? s "") (equal? (substring s 0 1) ";")))
    (regexp-match* #px"[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;[^\n]*|[^\\s\\[\\]{}('\"`,;)]*)"
                   str #:match-select cadr)))

(define (read_atom rdr)
  (let ([token (send rdr next)])
    (cond [(regexp-match #px"^-?[0-9]+$" token)
           (string->number token)]
          [(regexp-match #px"^-?[0-9][0-9.]*$" token)
           (string->number token)]
          [(regexp-match #px"^\".*\"$" token)
           (string-replace
             (string-replace
               (substring token 1 (- (string-length token) 1))
               "\\\"" "\"")
             "\\n" "\n")]
          [(regexp-match #px"^:" token) (_keyword (substring token 1))]
          [(equal? "nil" token) nil]
          [(equal? "true" token) #t]
          [(equal? "false" token) #f]
          [else (string->symbol token)])))

(define (read_list_entries rdr end)
  (let ([tok (send rdr peek)])
    (cond
        [(eq? tok '()) (raise (string-append "expected '" end "'"))]
        [(equal? end tok) '()]
        [else
          (cons (read_form rdr) (read_list_entries rdr end))])))

(define (read_list rdr start end)
  (let ([token (send rdr next)])
    (if (equal? start token)
      (let ([lst (read_list_entries rdr end)])
        (send rdr next)
        lst)
      (raise (string-append "expected '" start "'")))))

(define (read_form rdr)
  (let ([token (send rdr peek)])
    (if (null? token)
      (raise (make-blank-exn "blank line" (current-continuation-marks)))
      (cond
        [(equal? "'" token) (send rdr next) (list 'quote (read_form rdr))]
        [(equal? "`" token) (send rdr next) (list 'quasiquote (read_form rdr))]
        [(equal? "~" token) (send rdr next) (list 'unquote (read_form rdr))]
        [(equal? "~@" token) (send rdr next) (list 'splice-unquote (read_form rdr))]
        [(equal? "^" token) (send rdr next)
                            (let ([meta (read_form rdr)])
                              (list 'with-meta (read_form rdr) meta))]
        [(equal? "@" token) (send rdr next) (list 'deref (read_form rdr))]

        [(equal? ")" token) (raise "unexpected ')'")]
        [(equal? "(" token) (read_list rdr "(" ")")]
        [(equal? "]" token) (raise "unexpected ']'")]
        [(equal? "[" token) (list->vector (read_list rdr "[" "]"))]
        [(equal? "}" token) (raise "unexpected '}'")]
        [(equal? "{" token) (apply hash (read_list rdr "{" "}"))]
        [else (read_atom rdr)]))))

(define (read_str str)
  (read_form (new Reader% [tokens (tokenize str)])))
