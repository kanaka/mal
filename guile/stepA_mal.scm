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

(define (READ)
  (read_str (_readline "user> ")))

(define (eval_ast ast env)
  (define (_eval x) (EVAL x env))
  (match ast
    ((? symbol? sym) (env-has sym env))
    ((? list? lst) (map _eval lst))
    ((? vector? vec) (vector-map (lambda (i x) (_eval x)) vec))
    ((? hash-table? ht)
     ;; NOTE: we must allocate a new hashmap here to avoid any side-effects, or
     ;;       there'll be strange bugs!!!
     (list->hash-map (hash-fold (lambda (k v p) (cons k (cons (_eval v) p))) '() ht)))
    (else ast)))

(define (eval_func ast env)
  (define (_eval o) (EVAL o env))
  (define (func? x)
    (let ((f (if (list? x)
                 (EVAL x env)
                 x)))
      (if (callable? f)
          f
          (and=> (env-check f env) is-func?))))
  (cond
   ((func? (car ast))
    => (lambda (c)
         (callable-apply c (map _eval (cdr ast)))))
   (else (throw 'mal-error (format #f "'~a' not found" (car ast))))))

(define (eval_seq ast env)
  (cond
   ((null? ast) nil)
   ((null? (cdr ast)) (EVAL (car ast) env))
   (else
    (EVAL (car ast) env)
    (eval_seq (cdr ast) env))))

(define (is_macro_call ast env)
  (and (list? ast)
       (and=> (env-check (car ast) env) is-macro?)))

(define (_macroexpand ast env)
  (cond
   ((is_macro_call ast env)
    => (lambda (c)
         ;; NOTE: Macros are normal-order, so we shouldn't eval args here.
         ;;       Or it's applicable-order.
         (_macroexpand (callable-apply c (cdr ast)) env)))
   (else ast)))
   
(define (EVAL ast env)
  (define (%unzip2 kvs)
    (let lp((next kvs) (k '()) (v '()))
      (cond
       ;; NOTE: reverse is very important here!
       ((null? next) (values (reverse k) (reverse v)))
       ((null? (cdr next))
        (throw 'mal-error (format #f "let*: Invalid binding form '~a'" kvs))) 
       (else (lp (cddr next) (cons (car next) k) (cons (cadr next) v))))))
  (define (_quasiquote obj)
    (match obj
      ((('unquote unq) rest ...) `(cons ,unq ,(_quasiquote rest)))
      (('unquote unq) unq)
      ((('splice-unquote unqsp) rest ...) `(concat ,unqsp ,(_quasiquote rest)))
      ((head rest ...) (list 'cons (_quasiquote head) (_quasiquote rest)))
      (else `(quote ,obj))))
  ;; NOTE: I wish I can use (while #t ...) for that, but this is not Lispy, which means
  ;;       it'll bring some trouble in control flow. We have to use continuations to return
  ;;       and use non-standard `break' feature. In a word, not elegant at all.
  ;;       The named let loop is natural for Scheme, but it looks a bit cheating. But NO!
  ;;       Such kind of loop is actually `while loop' in Scheme, I don't take advantage of
  ;;       TCO in Scheme to implement TCO, but it's the same principle with normal loop.
  ;;       If you're Lispy enough, there's no recursive at all while you saw named let loop.
  (let tco-loop((ast ast) (env env)) ; expand as possible
    (let ((ast (_macroexpand ast env)))
      (match ast
        ((? non-list?) (eval_ast ast env))
        (('defmacro! k v)
         (let ((c (EVAL v env)))
           (callable-is_macro-set! c #t)
           ((env 'set) k c)))
        (('macroexpand obj) (_macroexpand obj env))
        (('quote obj) obj)
        (('quasiquote obj) (EVAL (_quasiquote (->list obj)) env))
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
        (('try* A ('catch* B C))
         (catch
          #t
          (lambda () (EVAL A env))
          (lambda (k . e)
            (case k
              ((mal-error)
               (let ((nenv (make-Env #:outer env #:binds (list B) #:exprs e)))
                 (EVAL C nenv)))
              ;; TODO: add backtrace
              (else (print-exception (current-output-port) #f k e))))))
        (else (eval_func ast env))))))

(define (EVAL-string str)
  (EVAL (read_str str) *toplevel*))

(define (PRINT exp)
  (and (not (eof-object? exp))
       (format #t "~a~%" (pr_str exp #t))))

(define (LOOP continue?)
  (and continue? (REPL)))

(define (REPL)
  (LOOP
   (catch #t
          (lambda () (PRINT (EVAL (READ) *toplevel*)))
          (lambda (k . e)
            (case k
              ((mal-error)
               (if (string=? (car e) "blank line")
                   (display "")
                   (format #t "Error: ~a~%" (car e))))
              (else (print-exception (current-output-port) #f k e)))))))

;; initialization
((*toplevel* 'set) 'eval (make-func (lambda (ast) (EVAL ast *toplevel*))))
((*toplevel* 'set) 'throw (make-func (lambda (val) (throw 'mal-error val))))
((*toplevel* 'set) '*ARGV* '())
(EVAL-string "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
(EVAL-string "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")
(EVAL-string "(def! *gensym-counter* (atom 0))")
(EVAL-string "(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))")
(EVAL-string "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))")
(EVAL-string "(def! *host-language* \"guile\")")

(let ((args (cdr (command-line))))
  (cond
   ((> (length args) 0)
    ((*toplevel* 'set) '*ARGV* (cdr args))
    (EVAL-string (string-append "(load-file \"" (car args) "\")")))
   (else
    (EVAL-string "(println (str \"Mal (\" *host-language* \")\"))")
    (REPL))))
