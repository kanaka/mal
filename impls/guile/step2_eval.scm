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

(import (readline) (reader) (printer) (ice-9 match) (srfi srfi-43) (types))

(define *toplevel*
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)))

(define (READ str)
  (read_str str))

(define (EVAL ast env)
  ; (format #t "EVAL: ~a~%" (pr_str ast #t))
  (match ast
    ((? symbol? sym)
     (or (assoc-ref env sym)
         (throw 'mal-error (format #f "'~a' not found" sym))))
    ((? vector? vec) (vector-map (lambda (i x) (EVAL x env)) vec))
    ((? hash-table? ht)
     (define new-ht (make-hash-table))
     (hash-for-each (lambda (k v) (hash-set! new-ht k (EVAL v env))) ht)
     new-ht)
    ((? non-list?) ast)
    (() ast)
    (else
      (let ((el (map (lambda (x) (EVAL x env)) ast)))
        (apply (car el) (cdr el))))))

(define (PRINT exp)
  (and (not (eof-object? exp))
       (format #t "~a~%" (pr_str exp #t))))

(define (LOOP continue?)
  (and continue? (REPL)))

(define (REPL)
  (LOOP
   (let ((line (_readline "user> ")))
     (cond
       ((eof-object? line) #f)
       ((string=? line "") #t)
       (else
         (catch 'mal-error
                (lambda () (PRINT (EVAL (READ line) *toplevel*)))
                (lambda (k . e)
                  (format #t "Error: ~a~%" (pr_str (car e) #t)))))))))

(REPL)
