(define-library (lib env)

(export make-env env-set env-find env-get)

(import (scheme base))

(import (lib util))
(import (lib types))

(begin

(define-record-type env
  (%make-env outer data)
  env?
  (outer env-outer)
  (data env-data env-data-set!))

(define (make-env outer . rest)
  (let ((env (%make-env outer '())))
    (when (pair? rest)
      (let loop ((binds (car rest))
                 (exprs (cadr rest)))
        (when (pair? binds)
          (let ((bind (car binds)))
            (if (eq? bind '&)
                (env-set env (cadr binds) (mal-list exprs))
                (begin
                  (env-set env bind (car exprs))
                  (loop (cdr binds) (cdr exprs))))))))
    env))

(define (env-set env key value)
  (env-data-set! env (cons (cons key value) (env-data env))))

(define (env-find env key)
  (cond
   ((alist-ref key (env-data env)) => identity)
   ((env-outer env) => (lambda (outer) (env-find outer key)))
   (else #f)))

(define (env-get env key)
  (let ((value (env-find env key)))
    (if value
        value
        (error (str "'" key "' not found")))))

)

)
