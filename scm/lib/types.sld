(define-library (lib types)

(export make-mal-object mal-object? mal-type mal-value mal-value-set! mal-meta
        mal-true mal-false mal-nil
        mal-number mal-string mal-symbol mal-keyword
        mal-list mal-vector mal-map mal-atom

        make-func func? func-ast func-params func-env
        func-fn func-macro? func-macro?-set! func-meta func-meta-set!

        mal-instance-of?)

(import (scheme base))

(begin

(define-record-type mal-object
  (make-mal-object type value meta)
  mal-object?
  (type mal-type)
  (value mal-value mal-value-set!)
  (meta mal-meta mal-meta-set!))

(define mal-true (make-mal-object 'true #t #f))
(define mal-false (make-mal-object 'false #f #f))
(define mal-nil (make-mal-object 'nil #f #f))

(define (mal-number n)
  (make-mal-object 'number n #f))

(define (mal-string string)
  (make-mal-object 'string string #f))

(define (mal-symbol name)
  (make-mal-object 'symbol name #f))

(define (mal-keyword name)
  (make-mal-object 'keyword name #f))

(define (mal-list items)
  (make-mal-object 'list items #f))

(define (mal-vector items)
  (make-mal-object 'vector items #f))

(define (mal-map items)
  (make-mal-object 'map items #f))

(define (mal-atom item)
  (make-mal-object 'atom item #f))

(define-record-type func
  (%make-func ast params env fn macro? meta)
  func?
  (ast func-ast)
  (params func-params)
  (env func-env)
  (fn func-fn)
  (macro? func-macro? func-macro?-set!)
  (meta func-meta func-meta-set!))

(define (make-func ast params env fn)
  (%make-func ast params env fn #f #f))

(define (mal-instance-of? x type)
  (and (mal-object? x) (eq? (mal-type x) type)))

)

)
