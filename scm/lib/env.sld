(define-library (lib env)

(export make-env env-set env-find env-get)

(import (scheme base))

(import (lib util))

(begin

(define-record-type env
  (%make-env outer data)
  env?
  (outer env-outer)
  (data env-data env-data-set!))

(define (make-env outer . rest)
  (let ((env (%make-env outer '())))
    (when (pair? rest)
      (let ((binds (car rest))
            (exprs (cadr rest)))
        (for-each (lambda (bind expr) (env-set env bind expr))
                  binds
                  exprs)))
    env))

(define (env-set env key value)
  (env-data-set! env (cons (cons key value) (env-data env))))

(define (env-find env key)
  (cond
   ((alist-ref key (env-data env)) env)
   ((env-outer env) => (lambda (outer) (env-find outer key)))
   (else #f)))

(define (env-get env key)
  (let ((env (env-find env key)))
    (if env
        (alist-ref key (env-data env))
        (error (str "'" key "' not found")))))

)

)
