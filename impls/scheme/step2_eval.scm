(import (scheme base))
(import (scheme write))

(import (lib util))
(import (lib reader))
(import (lib printer))
(import (lib types))

(define (READ input)
  (read-str input))

(define (EVAL ast env)
    ; (display (str "EVAL: " (pr-str ast #t) "\n"))
    (case (and (mal-object? ast) (mal-type ast))
      ((symbol)
       (let ((key (mal-value ast)))
         (or (alist-ref key env) (error (str "'" key "' not found")))))
      ((vector)
       (mal-vector (vector-map (lambda (item) (EVAL item env))
                               (mal-value ast))))
      ((map)
       (mal-map (alist-map (lambda (key value) (cons key (EVAL value env)))
                           (mal-value ast))))
      ((list)
        (let ((items (mal-value ast)))
        (if (null? items)
            ast
            (let ((op (EVAL (car items) env))
                  (ops (map (lambda (item) (EVAL item env)) (cdr items))))
              (apply op ops)))))
      (else ast)))

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
