#!/usr/bin/env racket
#lang racket

(require "readline.rkt" "types.rkt" "reader.rkt" "printer.rkt"
         "env.rkt" "core.rkt")

;; read
(define (READ str)
  (read_str str))

;; eval
(define (eval-ast ast env)
  (cond
    [(symbol? ast) (send env get ast)]
    [(_sequential? ast) (_map (lambda (x) (EVAL x env)) ast)]
    [(hash? ast) (make-hash
                  (dict-map ast (lambda (k v) (cons k (EVAL v env)))))]
    [else ast]))

(define (EVAL ast env)
  (if (not (list? ast))
        (eval-ast ast env)

        (let ([a0 (_nth ast 0)])
          (cond
            [(eq? 'def! a0)
             (send env set (_nth ast 1) (EVAL (_nth ast 2) env))]
            [(eq? 'let* a0)
             (let ([let-env (new Env% [outer env] [binds null] [exprs null])])
               (_map (lambda (b_e)
                       (send let-env set (_first b_e)
                             (EVAL (_nth b_e 1) let-env)))
                    (_partition 2 (_to_list (_nth ast 1))))
               (EVAL (_nth ast 2) let-env))]
            [(eq? 'do a0)
             (last (eval-ast (rest ast) env))]
            [(eq? 'if a0)
             (let ([cnd (EVAL (_nth ast 1) env)])
               (if (or (eq? cnd nil) (eq? cnd #f))
                 (if (> (length ast) 3)
                   (EVAL (_nth ast 3) env)
                   nil)
                 (EVAL (_nth ast 2) env)))]
            [(eq? 'fn* a0)
               (lambda args (EVAL (_nth ast 2)
                                  (new Env% [outer env]
                                            [binds (_nth ast 1)]
                                            [exprs args])))]
            [else (let* ([el (eval-ast ast env)]
                         [f (first el)]
                         [args (rest el)])
                    (apply f args))]))))

;; print
(define (PRINT exp)
  (pr_str exp true))

;; repl
(define repl-env
  (new Env% [outer null] [binds null] [exprs null]))
(define (rep str)
  (PRINT (EVAL (READ str) repl-env)))

(for ()  ;; ignore return values

;; core.rkt: defined using Racket
(hash-for-each core_ns (lambda (k v) (send repl-env set k v)))

;; core.mal: defined using the language itself
(rep "(def! not (fn* (a) (if a false true)))")

)

(define (repl-loop)
  (let ([line (readline "user> ")])
    (when (not (eq? nil line))
      (with-handlers
        ([string? (lambda (exc) (printf "Error: ~a~n" exc))]
         [blank-exn? (lambda (exc) null)])
        (printf "~a~n" (rep line)))
      (repl-loop))))
(repl-loop)
