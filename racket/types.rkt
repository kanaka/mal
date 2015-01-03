#lang racket

(provide blank-exn? make-blank-exn mal-exn? make-mal-exn mal-exn-val
         malfunc malfunc? malfunc-fn
         malfunc-ast malfunc-env malfunc-params malfunc-macro? malfunc-meta
         _partition _equal? _printf
         nil _nil? _keyword _keyword?
         _to_list _sequential? _count _empty? _nth _first _rest _map
         _assoc _dissoc _get
         atom atom? atom-val set-atom-val!)

(define-struct (blank-exn exn:fail:user) ())
(define-struct (mal-exn exn:fail:user) [val])

(define nil%
  (class object%
    (super-new)))

(define nil (new nil%))

(define (_nil? obj)
  (eq? nil obj))

(struct malfunc [fn ast env params macro? meta]
        #:property prop:procedure (struct-field-index fn))

;; General functions

;; From: http://stackoverflow.com/questions/8725832/how-to-split-list-into-evenly-sized-chunks-in-racket-scheme/8731622#8731622
(define (_partition n xs)
  (if (null? xs)
      '()
      (let ((first-chunk (take xs n))
            (rest (drop xs n)))
        (cons first-chunk (_partition n rest)))))

(define (_equal? a b)
  (cond
    [(and (list? a) (vector? b))
     (equal? a (vector->list b))]
    [(and (vector? a) (list? b))
     (equal? (vector->list a) b)]
    [else (equal? a b)]))

;; printf with flush
(define _printf (lambda a (apply printf a) (flush-output)))

;; Keywords
(define (_keyword str)
  (string-append "\u029e" str))

(define (_keyword? k)
  (and (string? k) (regexp-match? #px"^\u029e" k)))


;; Lists and vectors

(define (_to_list a)
  (if (vector? a) (vector->list a) a))

(define (_sequential? seq)
  (or (vector? seq) (list? seq)))

(define (_count seq)
  (cond [(_nil? seq)    0]
        [(vector? seq) (vector-length seq)]
        [else          (length seq)]))

(define (_empty? seq)
  (eq? 0 (_count seq)))

(define (_nth seq idx)
    (cond [(>= idx (_count seq)) (raise "nth: index out of range")]
          [(vector? seq) (vector-ref seq idx)]
          [else          (list-ref seq idx)]))

(define (_first seq)
  (cond [(vector? seq) (if (_empty? seq) nil (vector-ref seq 0))]
        [else          (if (_empty? seq) nil (list-ref seq 0))]))

(define (_rest seq)
  (cond [(vector? seq) (if (_empty? seq) '() (rest (vector->list seq)))]
        [else          (if (_empty? seq) '() (rest seq))]))

(define (_map f seq)
  (cond [(vector? seq) (vector-map f seq)]
        [else          (map f seq)]))

;; Hash maps
(define _assoc
  (lambda args
    (let ([new-hm (hash-copy (first args))]
          [pairs (_partition 2 (rest args))])
      (map (lambda (k_v)
             (hash-set! new-hm (first k_v) (second k_v))) pairs)
      new-hm)))

(define _dissoc
  (lambda args
    (let ([new-hm (hash-copy (first args))])
      (map (lambda (k) (hash-remove! new-hm k)) (rest args))
      new-hm)))

(define (_get hm k)
    (cond [(_nil? hm) nil]
          [(dict-has-key? hm k) (hash-ref hm k)]
          [else nil]))

;; Atoms
(struct atom [val] #:mutable)
