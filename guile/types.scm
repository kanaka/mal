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

(library (types)
  (export string-sub *eof* non-list?
          string->keyword _keyword?
          nil _nil?
          cond-true?
          atom atom? atom-val atom-val-set!
          make-callable callable? callable-is_macro
          callable-is_macro-set! callable-closure
          is-func? is-macro? make-func callable-apply
          hash-table-clone)
  (import (guile) (rnrs) (ice-9 regex) (ice-9 session)))

(define (non-list? x) (not (list? x)))


(define (string-sub str p1 p2)
  (regexp-substitute/global #f p1 str 'pre p2 'post))

(define *eof* (call-with-input-string "" read))

(define (string->keyword str)
  (when (not (string? str))
    (throw 'mal-error (format #f "string->keyword: '~a' is not a string" str)))
  (string-append "\u029e" str))

(define (_keyword? k)
  (and (string? k) (if (string-match "^\u029e" k) #t #f)))

(define nil 'nil)

(define (_nil? obj) (eq? nil obj))

(define (cond-true? obj)
  (and (not (_nil? obj)) obj))

(define-record-type atom (fields (mutable val)))

(define-record-type callable
  (fields
   (mutable is_macro)
   closure))

(define (make-func closure) (make-callable #f closure))

(define (callable-apply c arglst)
  (define closure (callable-closure c))
  ;;(format #t "ZZZ: ~a~%" `(apply ,closure ,arglst))
  (apply closure arglst))

(define (callable-check c b)
  (and (callable? c)
       (eq? (callable-is_macro c) b)
       c))

(define (is-func? c) (callable-check c #f))
(define (is-macro? c) (callable-check c #t))

(define (hash-table-clone ht)
  (define cht (make-hash-table))
  (hash-for-each (lambda (k v) (hash-set! cht k v)) ht)
  cht)
