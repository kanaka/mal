;;  Copyright (C) 2015
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(library (reader)
         (export read_str)
         (import (guile) (pcre) (ice-9 match) (ice-9 regex) (types)))

(define (make-Reader tokens)
  (lambda (cmd)
    (case cmd
      ((next)
       (if (null? tokens)
           '()
           (let ((r (car tokens))) (set! tokens (cdr tokens)) r)))
      ((peek) (if (null? tokens) '() (car tokens)))
      (else (error "Reader: Invalid cmd!" cmd)))))

(define *token-re*
  (new-pcre "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;[^\n]*|[^\\s\\[\\]{}('\"`,;)]*)"))

(define (tokenizer str)
  (pcre-search *token-re* str))

(define (delim-read reader delim)
  (let lp((next (reader 'next)) (ret '()))
    (cond
     ((null? next) (throw 'parse-error (format #f "expect '~a'!" delim)))
     ((string=? next delim) (reverse ret))
     (else
      (let ((n (reader 'next)))
        (lp n (cons (read_form reader) ret)))))))

(define (read_list reader)
  (if (string=? ")" (reader 'next))
      '()
      (delim-read reader ")")))

(define (read_vector reader)
  (if (string=? "]" (reader 'next))
      #()
      (list->vector (delim-read reader "]"))))

(define (read_hashmap reader)
  (define ht (make-hash-table))
  (define lst (delim-read reader "}"))
  (if (string=? "}" (reader 'next))
      ht
      (let lp((k (car lst)))
        (cond
         ((null? k) ht)
         (else
          (let ((v (reader 'next)))
            (when (null? v)
                  (throw 'parse-error "read_hashmap: lack of value" k))
            (hash-set! ht k v)
            (lp (reader 'next))))))))

(define (read_atom reader)
  (let ((token (reader 'next)))
    (cond
     ((string-match "^-?[0-9][0-9.]*$" token)
      => (lambda (m) (string->number (match:substring m 0))))
     ((string-match "^\"(.*)\"$" token)
      => (lambda (m) (match:substring m 1)))
     ((string-match "^:(.*)" token)
      => (lambda (m) (_keyword (match:substring m 1))))
     ((string=? "nil" token) nil)
     ((string=? "true" token) #t)
     ((string=? "false" token) #f)
     (else (string->symbol token)))))

(define (read_form reader)
  (define (next) (reader 'next))
  (define (more) (read_form reader))
  (match (reader 'peek)
    (() *eof*) ; FIXME: what should be returned?
    ("'" (next) (list 'quote (more)))
    ("`" (next) (list 'quasiquote (more)))
    ("~" (next) (list 'unquote (more)))
    ("~@" (next) (list 'splice-unquote (more)))
    ("^" (next) (let ((meta (more))) `(with-meta ,(more) ,meta)))
    ("@" (next) `(deref ,(more)))
    (")" (next) (throw 'parse-error "unexpected ')'"))
    ("(" (read_list reader))
    ("]" (throw 'parse-error "unexpected ']'"))
    ("[" (read_vector reader))
    ("}" (throw 'parse-error "unexpected '}'"))
    ("{" (read_hashmap reader))
    (else (read_atom reader))))

(define (read_str str)
  (if (eof-object? str)
      str
      (read_form (make-Reader (tokenizer str)))))
