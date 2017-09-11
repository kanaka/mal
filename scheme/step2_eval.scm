(import (scheme base))
(import (scheme write))

(import (lib util))
(import (lib reader))
(import (lib printer))
(import (lib types))

(define (READ input)
  (read-str input))

(define (eval-ast ast env)
  (let ((type (and (mal-object? ast) (mal-type ast)))
        (value (and (mal-object? ast) (mal-value ast))))
    (case type
      ((symbol) (or (alist-ref value env)
                    (error (str "'" value "' not found"))))
      ((list) (mal-list (map (lambda (item) (EVAL item env)) value)))
      ((vector) (mal-vector (vector-map (lambda (item) (EVAL item env)) value)))
      ((map) (mal-map (alist-map (lambda (key value) (cons key (EVAL value env))) value)))
      (else ast))))

(define (EVAL ast env)
  (let ((type (and (mal-object? ast) (mal-type ast))))
    (if (not (eq? type 'list))
        (eval-ast ast env)
        (let ((items (mal-value ast)))
        (if (null? items)
            ast
            (let* ((items (mal-value (eval-ast ast env)))
                   (op (car items))
                   (ops (cdr items)))
              (apply op ops)))))))

(define (PRINT ast)
  (pr-str ast #t))

(define repl-env
  `((+ . ,(lambda (a b) (mal-number (+ (mal-value a) (mal-value b)))))
    (- . ,(lambda (a b) (mal-number (- (mal-value a) (mal-value b)))))
    (* . ,(lambda (a b) (mal-number (* (mal-value a) (mal-value b)))))
    (/ . ,(lambda (a b) (mal-number (/ (mal-value a) (mal-value b)))))))

(define (rep input)
  (PRINT (EVAL (READ input) repl-env)))

(define (main)
  (let loop ()
    (let ((input (readline "user> ")))
      (when input
        (guard
         (ex ((error-object? ex)
              (when (not (memv 'empty-input (error-object-irritants ex)))
                (display "[error] ")
                (display (error-object-message ex))
                (newline))))
         (display (rep input))
         (newline))
        (loop))))
  (newline))

(main)
