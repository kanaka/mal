#lang racket

(provide pr_str pr_lst)

(require "types.rkt")

(define (pr_str obj print_readably)
  (let ([_r print_readably])
    (cond
      [(list? obj)
       (string-join (map (lambda (o) (pr_str o _r)) obj)
                    " " #:before-first "(" #:after-last ")")]
      [(vector? obj)
       (string-join (map (lambda (o) (pr_str o _r)) (vector->list obj))
                    " " #:before-first "[" #:after-last "]")]
      [(hash? obj)
       (string-join (dict-map obj (lambda (k v)
                                    (format "~a ~a"
                                            (pr_str k _r)
                                            (pr_str v _r))))
                    " " #:before-first "{" #:after-last "}")]
      [(string? obj)
       (if (regexp-match #px"^\u029e" obj)
         (format ":~a" (substring obj 1))
         (if _r
           (format "\"~a\""
             (string-replace
               (string-replace
                 (string-replace obj "\\" "\\\\")
                 "\"" "\\\"")
                "\n" "\\n"))
           obj))]
      [(number? obj) (number->string obj)]
      [(symbol? obj) (symbol->string obj)]
      [(atom? obj) (format "(atom ~a)" (atom-val obj))]
      [(_nil? obj) "nil"]
      [(eq? #t obj) "true"]
      [(eq? #f obj) "false"]
      [else (format "~a" obj)])))

(define (pr_lst lst print_readably sep)
  (string-join
    (map (lambda (s) (pr_str s print_readably)) lst)
    sep))
