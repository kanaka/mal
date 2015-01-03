#!/usr/bin/env racket
#lang racket

(require "types.rkt")

;; read
(define (READ str)
  str)

;; eval
(define (EVAL ast env)
  ast)

;; print
(define (PRINT exp)
  exp)

;; repl
(define (rep str)
  (PRINT (EVAL (READ str) "")))

(define (repl-loop)
  (let ([line (readline "user> ")])
    (when (not (eq? nil line))
      (printf "~a~n" (rep line))
      (repl-loop))))
(repl-loop)
