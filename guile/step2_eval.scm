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

(import (readline) (reader) (printer) (ice-9 match) (srfi srfi-43))

(define *toplevel*
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)))

(define (READ str)
  (read_str str))

(define (eval_ast ast env)
  (define (_eval x) (EVAL x env))
  (match ast
    ((? symbol? sym)
     (or (assoc-ref env sym)
         (throw 'mal-error (format #f "'~a' not found" sym))))
    ((? list? lst) (map _eval lst))
    ((? vector? vec) (vector-map (lambda (i x) (_eval x)) vec))
    ((? hash-table? ht)
     (hash-for-each (lambda (k v) (hash-set! ht k (_eval v))) ht)
     ht)
    (else ast)))

(define (EVAL ast env)
  (match ast
    (() ast)
    ((? list?)
     (let ((el (eval_ast ast env)))
        (apply (car el) (cdr el))))
    (else (eval_ast ast env))))

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
