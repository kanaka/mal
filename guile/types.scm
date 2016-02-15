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
          string->keyword _keyword? _string?
          nil _nil? list->hash-map
          cond-true? make-anonymous-func
          make-atom atom? atom-val atom-val-set!
          make-callable callable? callable-is_macro
          callable-is_macro-set! callable-closure
          is-func? is-macro? make-func callable-apply
          callable-unbox-set! callable-unbox
          callable-meta-info hash-table-clone
          box? box unbox)
  (import (guile) (only (rnrs) define-record-type) (ice-9 regex) (ice-9 session)))

(define (non-list? x) (not (list? x)))


(define (string-sub str p1 p2)
  (regexp-substitute/global #f p1 str 'pre p2 'post))

(define *eof* (call-with-input-string "" read))

(define (string->keyword str)
  (when (not (string? str))
    (throw 'mal-error (format #f "string->keyword: '~a' is not a string" str)))
  (string-append "\u029e" str))

(define (_keyword? k)
  (and (string? k)
       (> (string-length k) 0)
       (char=? #\1236 (string-ref k 0))))

(define (_string? s)
  (and (string? s) (not (_keyword? s))))

(define-record-type mal-nil)

(define nil (make-mal-nil))

(define (_nil? obj) (mal-nil? obj))

(define (cond-true? obj)
  (and (not (_nil? obj)) obj))

(define-record-type atom (fields (mutable val)))

(define-record-type callable
  (fields
   meta-info
   (mutable unbox)
   (mutable is_macro)
   closure))

(define (make-func closure) (make-callable nil #t #f closure))
(define (make-anonymous-func closure) (make-callable nil #f #f closure))

(define (callable-apply c arglst)
  (apply (callable-closure c) (if (callable-unbox c) (map unbox arglst) arglst)))

(define (callable-check c b)
  (and (callable? c)
       (eq? (callable-is_macro c) b)
       c))

(define (is-func? c) (callable-check c #f))
(define (is-macro? c) (callable-check c #t))

(define (hash-table-clone ht)
  (list->hash-map (hash-fold (lambda (k v p) (cons k (cons v p))) '() ht)))

(define-record-type box (fields val))

(define (box o) (make-box o))
(define (unbox o)
  (if (box? o) (box-val o) o))

(define* (list->hash-map lst #:optional (ht (make-hash-table)))
  (cond
   ((null? lst) ht)
   (else
    (let lp((next lst))
      (cond
       ((null? next) ht)
       (else
        (when (null? (cdr next))
          (throw 'mal-error
                 (format #f "hash-map: '~a' lack of value" (car next))))
        (let ((k (car next))
              (v (cadr next)))
          (hash-set! ht k v)
          (lp (cddr next)))))))))
