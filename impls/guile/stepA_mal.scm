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

(import (readline) (reader) (printer) (ice-9 match) (srfi srfi-43)
        (srfi srfi-1) (ice-9 receive) (env) (core) (types))

;; Primitives which doesn't unbox args in default.
;; This is a trick to implement meta-info taking advange of the original
;; types of Guile as possible.
(define *unbox-exception* '(meta assoc swap!))

(define *toplevel*
  (receive (b e) (unzip2 core.ns)
    (let ((env (make-Env #:binds b #:exprs (map make-func e))))
      (for-each (lambda (f)
                  (callable-unbox-set! ((env 'get) f) #f))
                *unbox-exception*)
      env)))

(define (READ str)
  (read_str str))

(define (eval_seq ast env)
  (cond
   ((null? ast) nil)
   ((null? (cdr ast)) (EVAL (car ast) env))
   (else
    (EVAL (car ast) env)
    (eval_seq (cdr ast) env))))

(define (qqIter elt acc)
  (match elt
    (('splice-unquote x) (list 'concat x acc))
    (else                (list 'cons (_quasiquote elt) acc))))
(define (_quasiquote ast)
  (match ast
    (('unquote x)    x)
    ( (xs ...)       (fold-right qqIter '() xs))
    (#(xs ...)       (list 'vec (fold-right qqIter '() xs)))
    ((? hash-table?) (list 'quote ast))
    ((? symbol?)     (list 'quote ast))
    (else            ast)))

(define (EVAL ast env)
  (define (%unzip2 kvs)
    (let lp((next kvs) (k '()) (v '()))
      (cond
       ;; NOTE: reverse is very important here!
       ((null? next) (values (reverse k) (reverse v)))
       ((null? (cdr next))
        (throw 'mal-error (format #f "let*: Invalid binding form '~a'" kvs))) 
       (else (lp (cddr next) (cons (car next) k) (cons (cadr next) v))))))
  (let tco-loop((ast ast) (env env)) ; expand as possible
      (when (cond-true? (env-check 'DEBUG-EVAL env))
        (format #t "EVAL: ~a~%" (pr_str ast #t)))
      (match ast
        ((? symbol? sym) (env-has sym env))
        ((? vector? vec) (vector-map (lambda (i x) (EVAL x env)) vec))
        ((? hash-table? ht)
         (define new-ht (make-hash-table))
         (hash-for-each (lambda (k v) (hash-set! new-ht k (EVAL v env))) ht)
         new-ht)
        ((? non-list?) ast)
        (() ast)
        (('defmacro! k v)
         (let ((c (EVAL v env)))
           ((env 'set) k (callable-as-macro c))))
        (('quote obj) obj)
        (('quasiquote obj) (EVAL (_quasiquote obj) env))
        (('def! k v) ((env 'set) k (EVAL v env)))
        (('let* kvs body)
         (let* ((new-env (make-Env #:outer env))
                (setter (lambda (k v) ((new-env 'set) k (EVAL v new-env)))))
           (receive (keys vals) (%unzip2 (->list kvs))
                    (for-each setter keys vals))
           (tco-loop body new-env)))
        (('do rest ...)
         (cond
          ((null? rest)
           (throw 'mal-error (format #f "do: Invalid form! '~a'" rest)))
          ((= 1 (length rest)) (tco-loop (car rest) env))
          (else
           (let ((mexpr (take rest (1- (length rest))))
                 (tail-call (car (take-right rest 1))))
             (eval_seq mexpr env)
             (tco-loop tail-call env)))))
        (('if cnd thn els ...)
         (cond
          ((and (not (null? els)) (not (null? (cdr els))))
           ;; Invalid `if' form
           (throw 'mal-error
                  (format #f "if: failed to match any pattern in form '~a'" ast)))
          ((cond-true? (EVAL cnd env)) (tco-loop thn env))
          (else (if (null? els) nil (tco-loop (car els) env)))))
        (('fn* params body ...) ; function definition
         (make-anonymous-func
          (lambda args
            (let ((nenv (make-Env #:outer env #:binds (->list params) #:exprs args)))
              (cond
               ((null? body)
                (throw 'mal-error (format #f "fn*: bad lambda in form '~a'" ast)))
               ((= 1 (length body)) (tco-loop (car body) nenv))
               (else
                (let ((mexpr (take body (1- (length body))))
                      (tail-call (car (take-right body 1))))
                  (eval_seq mexpr nenv)
                  (tco-loop tail-call nenv))))))))
        (('try* A)
         (EVAL A env))
        (('try* A ('catch* B C))
         (catch
          #t
          (lambda () (EVAL A env))
          (lambda e
            (let ((nenv (make-Env #:outer env #:binds (list B) #:exprs (cdr e))))
              (EVAL C nenv)))))
        (else
          (let ((f (EVAL (car ast) env))
                (args (cdr ast)))
            (if (is-macro f)
              (EVAL (callable-apply f args) env)
              (callable-apply f (map (lambda (x) (EVAL x env)) args))))))))

(define (EVAL-string str)
  (EVAL (read_str str) *toplevel*))

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

;; initialization
((*toplevel* 'set) 'eval (make-func (lambda (ast) (EVAL ast *toplevel*))))
((*toplevel* 'set) 'throw (make-func (lambda (val) (throw 'mal-error val))))
((*toplevel* 'set) '*ARGV* '())
(EVAL-string "(def! not (fn* (x) (if x false true)))")
(EVAL-string "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
(EVAL-string "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
(EVAL-string "(def! *host-language* \"guile\")")

(let ((args (cdr (command-line))))
  (cond
   ((> (length args) 0)
    ((*toplevel* 'set) '*ARGV* (cdr args))
    (EVAL-string (string-append "(load-file \"" (car args) "\")")))
   (else
    (EVAL-string "(println (str \"Mal (\" *host-language* \")\"))")
    (REPL))))
