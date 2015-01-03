#lang racket

(provide core_ns)

(require "readline.rkt" "types.rkt" "reader.rkt" "printer.rkt")

(define (throw exc)
  (raise (make-mal-exn "mal exception"
                       (current-continuation-marks)
                       exc)))

;; Sequence functions
(define conj
  (lambda a
    (if (vector? (first a))
      (vector-append (first a) (list->vector (rest a)))
      (append (reverse (rest a)) (first a)))))

;; Meta functions
(define (meta obj)
  (cond [(malfunc? obj) (malfunc-meta obj)]
        [else nil]))

(define (with-meta obj m)
  (cond [(malfunc? obj) (struct-copy malfunc obj [meta m])]
        [else (raise "metadata not supported on type")]))

;; Atom functions

(define swap!
  (lambda a
    (let* ([atm (first a)]
           [f (second a)]
           [args (cons (atom-val atm) (rest (rest a)))]
           [val (apply f args)])
      (set-atom-val! atm val)
      val)))

(define core_ns
  (hash
    '=        _equal?
    'throw    throw

    'nil?     _nil?
    'true?    (lambda (x) (eq? x #t))
    'false?   (lambda (x) (eq? x #f))
    'symbol   (lambda (s) (if (symbol? s) s (string->symbol s)))
    'symbol?  symbol?
    'keyword  (lambda (s) (if (_keyword? s) s (_keyword s)))
    'keyword? _keyword?

    'pr-str   (lambda a (pr_lst a #t " "))
    'str      (lambda a (pr_lst a #f ""))
    'prn      (lambda a (printf "~a~n" (pr_lst a #t " ")) nil)
    'println  (lambda a (printf "~a~n" (pr_lst a #f " ")) nil)
    'read-string (lambda (s) (read_str s))
    'readline readline
    'slurp    (lambda (f) (port->string (open-input-file f)))

    '<        <
    '<=       <=
    '>        >
    '>=       >=
    '+        +
    '-        -
    '*        *
    '/        /
    'time-ms  (lambda () (round (current-inexact-milliseconds)))

    'list     list
    'list?    list?
    'vector   vector
    'vector?  vector?
    'hash-map hash
    'map?     hash?
    'assoc    _assoc
    'dissoc   _dissoc
    'get      _get
    'contains? dict-has-key?
    'keys     hash-keys
    'vals     hash-values

    'sequential? _sequential?
    'cons     (lambda a (cons (first a) (_to_list (second a))))
    'concat   (lambda a (apply append (map _to_list a)))
    'nth      _nth
    'first    _first
    'rest     _rest
    'empty?   _empty?
    'count    _count
    'apply    apply
    'map      (lambda (f s) (_to_list (_map f s)))
    'conj     conj

    'meta     meta
    'with-meta with-meta
    'atom     atom
    'atom?    atom?
    'deref    (lambda (a) (atom-val a))
    'reset!   (lambda (a v) (set-atom-val! a v) v)
    'swap!    swap!))
