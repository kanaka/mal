(import (scheme base))
(import (scheme write))

(import (lib util))
(import (lib reader))
(import (lib printer))
(import (lib types))
(import (lib env))
(import (lib core))

(define (READ input)
  (read-str input))

(define (eval-ast ast env)
  (let ((type (and (mal-object? ast) (mal-type ast)))
        (value (and (mal-object? ast) (mal-value ast))))
    (case type
      ((symbol) (env-get env value))
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
              (case (mal-value (car items))
                ((def!)
                 (let ((symbol (mal-value (cadr items)))
                       (value (EVAL (list-ref items 2) env)))
                   (env-set env symbol value)
                   value))
                ((let*)
                 (let ((env* (make-env env))
                       (binds (->list (mal-value (cadr items))))
                       (form (list-ref items 2)))
                   (let loop ((binds binds))
                     (when (pair? binds)
                       (let ((key (mal-value (car binds))))
                         (when (null? (cdr binds))
                           (error "unbalanced list"))
                         (let ((value (EVAL (cadr binds) env*)))
                           (env-set env* key value)
                           (loop (cddr binds))))))
                   (EVAL form env*))) ; TCO
                ((do)
                 (let ((forms (cdr items)))
                   (if (null? forms)
                       mal-nil
                       ;; the evaluation order of map is unspecified
                       (let loop ((forms forms))
                         (let ((form (car forms))
                               (tail (cdr forms)))
                           (if (null? tail)
                               (EVAL form env) ; TCO
                               (begin
                                 (EVAL form env)
                                 (loop tail))))))))
                ((if)
                 (let* ((condition (EVAL (cadr items) env))
                        (type (and (mal-object? condition)
                                   (mal-type condition))))
                   (if (memq type '(false nil))
                       (if (< (length items) 4)
                           mal-nil
                           (EVAL (list-ref items 3) env)) ; TCO
                       (EVAL (list-ref items 2) env)))) ; TCO
                ((fn*)
                 (let* ((binds (->list (mal-value (cadr items))))
                        (binds (map mal-value binds))
                        (body (list-ref items 2))
                        (fn (lambda args
                              (let ((env* (make-env env binds args)))
                                (EVAL body env*)))))
                   (make-func body binds env fn)))
                (else
                 (let* ((items (mal-value (eval-ast ast env)))
                        (op (car items))
                        (ops (cdr items)))
                   (if (func? op)
                       (let* ((outer (func-env op))
                              (binds (func-params op))
                              (env* (make-env outer binds ops)))
                         (EVAL (func-ast op) env*)) ; TCO
                       (apply op ops))))))))))

(define (PRINT ast)
  (pr-str ast #t))

(define repl-env (make-env #f))
(for-each (lambda (kv) (env-set repl-env (car kv) (cdr kv))) ns)

(define (rep input)
  (PRINT (EVAL (READ input) repl-env)))

(rep "(def! not (fn* (a) (if a false true)))")

(define (main)
  (let loop ()
    (let ((input (readline "user> ")))
      (when input
        (guard
         (ex ((error-object? ex)
              (when (not (memv 'empty-input (error-object-irritants ex)))
                (display "[error] ")
                (display (error-object-message ex))
                (newline)))
             ((and (pair? ex) (eq? (car ex) 'user-error))
              (display "[error] ")
              (display (pr-str (cdr ex) #t))
              (newline)))
         (display (rep input))
         (newline))
        (loop))))
  (newline))

(main)
