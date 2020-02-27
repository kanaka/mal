#!/usr/bin/env racket
#lang racket

(require "types.rkt" "readline.rkt" "reader.rkt" "printer.rkt")

;; read
(define (READ str)
  (read_str str))

;; eval
(define (eval-ast ast env)
  (cond
    [(symbol? ast)
     (or (hash-ref env ast
                   (lambda () (raise (string-append "'"
                                                    (symbol->string ast)
                                                    "' not found")))))]
    [(_sequential? ast) (_map (lambda (x) (EVAL x env)) ast)]
    [(hash? ast) (make-hash
                  (dict-map ast (lambda (k v) (cons k (EVAL v env)))))]
    [else ast]))

(define (EVAL ast env)
  (if (or (not (list? ast)) (empty? ast))
        (eval-ast ast env)

        (let* ([el (eval-ast ast env)]
               [f (first el)]
               [args (rest el)])
          (apply f args))))

;; print
(define (PRINT exp)
  (pr_str exp true))

;; repl
(define repl-env (hash '+ + '- - '* * '/ /))
(define (rep str)
  (PRINT (EVAL (READ str) repl-env)))

(define (repl-loop)
  (let ([line (readline "user> ")])
    (when (not (eq? nil line))
      (with-handlers
        ([string? (lambda (exc) (printf "Error: ~a~n" exc))]
         [blank-exn? (lambda (exc) null)])
        (printf "~a~n" (rep line)))
      (repl-loop))))
(repl-loop)
