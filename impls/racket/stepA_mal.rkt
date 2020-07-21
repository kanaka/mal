#!/usr/bin/env racket
#lang racket

(require "readline.rkt" "types.rkt" "reader.rkt" "printer.rkt"
         "env.rkt" "core.rkt")

;; read
(define (READ str)
  (read_str str))

;; eval

(define (qq-loop elt acc)
  (if (and (list? elt) (= (length elt) 2) (equal? (car elt) 'splice-unquote))
    (list 'concat (cadr elt) acc)
    (list 'cons (quasiquote elt) acc)))

(define (quasiquote ast)
  (cond
    [(or (symbol? ast) (hash? ast))
     (list 'quote ast)]

    [(vector? ast)
     (list 'vec (foldr qq-loop null (_to_list ast)))]

    [(not (list? ast))
     ast]

    [(and (= (length ast) 2) (equal? (car ast) 'unquote))
     (cadr ast)]

    [else
     (foldr qq-loop null ast)]))

(define (macro? ast env)
  (and (list? ast)
       (not (empty? ast))
       (symbol? (first ast))
       (not (equal? null (send env find (first ast))))
       (let ([fn (send env get (first ast))])
         (and (malfunc? fn) (malfunc-macro? fn)))))

(define (macroexpand ast env)
  (if (macro? ast env)
    (let ([mac (malfunc-fn (send env get (first ast)))])
      (macroexpand (apply mac (rest ast)) env))
    ast))

(define (eval-ast ast env)
  (cond
    [(symbol? ast) (send env get ast)]
    [(_sequential? ast) (_map (lambda (x) (EVAL x env)) ast)]
    [(hash? ast) (make-hash
                  (dict-map ast (lambda (k v) (cons k (EVAL v env)))))]
    [else ast]))

(define (EVAL ast env)
  ;(printf "~a~n" (pr_str ast true))
  (if (not (list? ast))
    (eval-ast ast env)

    (let ([ast (macroexpand ast env)])
      (if (or (not (list? ast)) (empty? ast))
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
            [(eq? 'quote a0)
             (_nth ast 1)]
            [(eq? 'quasiquoteexpand a0)
             (quasiquote (cadr ast))]
            [(eq? 'quasiquote a0)
             (EVAL (quasiquote (_nth ast 1)) env)]
            [(eq? 'defmacro! a0)
             (let* ([func (EVAL (_nth ast 2) env)]
                    [mac (struct-copy malfunc func [macro? #t])])
               (send env set (_nth ast 1) mac))]
            [(eq? 'macroexpand a0)
             (macroexpand (_nth ast 1) env)]
            [(eq? 'try* a0)
             (if (or (< (length ast) 3)
                     (not (eq? 'catch* (_nth (_nth ast 2) 0))))
               (EVAL (_nth ast 1) env)
               (let ([efn (lambda (exc)
                            (EVAL (_nth (_nth ast 2) 2)
                                  (new Env%
                                       [outer env]
                                       [binds (list (_nth (_nth ast 2) 1))]
                                       [exprs (list exc)])))])
                 (with-handlers
                   ([mal-exn?  (lambda (exc) (efn (mal-exn-val exc)))]
                    [string?   (lambda (exc) (efn exc))]
                    [exn:fail? (lambda (exc) (efn (format "~a" exc)))])
                   (EVAL (_nth ast 1) env))))]
            [(eq? 'do a0)
             (eval-ast (drop (drop-right ast 1) 1) env)
             (EVAL (last ast) env)]
            [(eq? 'if a0)
             (let ([cnd (EVAL (_nth ast 1) env)])
               (if (or (eq? cnd nil) (eq? cnd #f))
                 (if (> (length ast) 3)
                   (EVAL (_nth ast 3) env)
                   nil)
                 (EVAL (_nth ast 2) env)))]
            [(eq? 'fn* a0)
             (malfunc
               (lambda args (EVAL (_nth ast 2)
                                  (new Env% [outer env]
                                            [binds (_nth ast 1)]
                                            [exprs args])))
               (_nth ast 2) env (_nth ast 1) #f nil)]
            [else (let* ([el (eval-ast ast env)]
                         [f (first el)]
                         [args (rest el)])
                    (if (malfunc? f)
                      (EVAL (malfunc-ast f)
                            (new Env%
                                 [outer (malfunc-env f)]
                                 [binds (malfunc-params f)]
                                 [exprs args]))
                      (apply f args)))]))))))

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
(send repl-env set 'eval (lambda [ast] (EVAL ast repl-env)))
(send repl-env set '*ARGV* (_rest (current-command-line-arguments)))

;; core.mal: defined using the language itself
(rep "(def! *host-language* \"racket\")")
(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
(rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")

)

(define (repl-loop)
  (let ([line (readline "user> ")])
    (when (not (eq? nil line))
      (with-handlers
        ([string? (lambda (exc) (printf "Error: ~a~n" exc))]
         [mal-exn? (lambda (exc) (printf "Error: ~a~n"
                                         (pr_str (mal-exn-val exc) true)))]
         [blank-exn? (lambda (exc) null)])
        (printf "~a~n" (rep line)))
      (repl-loop))))
(let ([args (current-command-line-arguments)])
  (if (> (vector-length args) 0)
    (begin
      (send repl-env set '*ARGV* (vector->list (vector-drop args 1)))
      (for () (rep (string-append "(load-file \"" (vector-ref args 0) "\")"))))
    (begin
      (rep "(println (str \"Mal [\" *host-language* \"]\"))")
      (repl-loop))))
