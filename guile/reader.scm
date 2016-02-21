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
         (import (guile) (pcre) (ice-9 match) (srfi srfi-1)
                 (ice-9 regex) (types) (ice-9 format)))

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
  (filter (lambda (s) (and (not (string-null? s)) (not (string=? (substring s 0 1) ";"))))
          (pcre-search *token-re* str)))

(define (delim-read reader delim)
  (let lp((next (reader 'peek)) (ret '()))
    (cond
     ((null? next) (throw 'mal-error (format #f "expected '~a'" delim)))
     ((string=? next delim) (reader 'next) (reverse ret))
     (else
      (let* ((cur (read_form reader))
             (n (reader 'peek)))
        (lp n (cons cur ret)))))))

(define (read_list reader)
  (cond
   ((string=? ")" (reader 'peek))
    (reader 'next)
    '())
   (else (delim-read reader ")"))))

(define (read_vector reader)
  (cond
   ((string=? "]" (reader 'peek))
    (reader 'next)
    #())
   (else (list->vector (delim-read reader "]")))))

(define (read_hashmap reader)
  (define ht (make-hash-table))
  (define lst (delim-read reader "}"))
  (cond
   ((null? lst) ht)
   (else
    (let lp((next lst))
      (cond
       ((null? next) ht)
       (else
        (when (null? (cdr next))
          (throw 'mal-error
                 (format #f "read_hashmap: '~a' lack of value" (car next))))
        (let ((k (car next))
              (v (cadr next)))
          (hash-set! ht k v)
          (lp (cddr next)))))))))

(define (read_atom reader)
  (define (->str s)
    (string-sub
     (string-sub
      (string-sub s "\\\\\"" "\"")
      "\\\\n" "\n")
     "\\\\\\\\" "\\"))
  (let ((token (reader 'next)))
    (cond
     ((string-match "^-?[0-9][0-9.]*$" token)
      => (lambda (m) (string->number (match:substring m 0))))
     ((string-match "^\"(.*)(.)$" token)
      => (lambda (m)
           (if (string=? "\"" (match:substring m 2))
               (->str (match:substring m 1))
               (throw 'mal-error "expected '\"'"))))
     ((string-match "^:(.*)" token)
      => (lambda (m) (string->keyword (match:substring m 1))))
     ((string=? "nil" token) nil)
     ((string=? "true" token) #t)
     ((string=? "false" token) #f)
     (else (string->symbol token)))))

(define (read_form reader)
  (define (clean x)
    (if (string? x)
        (string-trim-both
         x
         (lambda (c) (char-set-contains? char-set:whitespace c)))
        x)) 
  (define (next) (reader 'next))
  (define (more) (read_form reader))
  (match (clean (reader 'peek)) 
    (() (throw 'mal-error "blank line")) ; FIXME: what should be returned?
    ("'" (next) (list 'quote (more)))
    ("`" (next) (list 'quasiquote (more)))
    ("~" (next) (list 'unquote (more)))
    ("~@" (next) (list 'splice-unquote (more)))
    ("^" (next) (let ((meta (more))) `(with-meta ,(more) ,meta)))
    ("@" (next) `(deref ,(more)))
    (")" (next) (throw 'mal-error "unexpected ')'"))
    ("(" (next) (read_list reader))
    ("]" (throw 'mal-error "unexpected ']'"))
    ("[" (next) (read_vector reader))
    ("}" (throw 'mal-error "unexpected '}'"))
    ("{" (next) (read_hashmap reader))
    ("" (next) (read_form reader))
    (else (read_atom reader))))

(define (read_str str)
  (if (eof-object? str)
      str
      (let* ((tokens (tokenizer str))
             (t (if (null? tokens)
                    (if (char=? (string-ref str 0) #\;)
                        '()
                        (list str))
                    tokens)))
        (read_form (make-Reader t)))))
