#!/usr/bin/env racket
#lang racket

(require "readline.rkt" "types.rkt" "reader.rkt" "printer.rkt")

;; read
(define (READ str)
  (read_str str))

;; eval
(define (EVAL ast env)
  ast)

;; print
(define (PRINT exp)
  (pr_str exp true))

;; repl
(define (rep str)
  (PRINT (EVAL (READ str) "")))

(define (repl-loop)
  (let ([line (readline "user> ")])
    (when (not (eq? nil line))
      (with-handlers
        ([string? (lambda (exc) (printf "Error: ~a~n" exc))]
         [blank-exn? (lambda (exc) null)])
        (printf "~a~n" (rep line)))
      (repl-loop))))
(repl-loop)
