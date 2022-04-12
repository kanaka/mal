#lang racket

(provide Env%)

(require "types.rkt")

(define Env%
  (class object%
    (init outer binds exprs)
    (super-new)
    (define _outer outer)
    (define _binds (_to_list binds))
    (define _exprs (_to_list exprs))
    (define data (make-hash))
    (let ([vargs (member '& _binds)])
      (if vargs
        (begin
          (map (lambda (b e) (hash-set! data b e))
               (drop-right _binds 2)
               (take _exprs (- (length _binds) 2)))
          (hash-set! data
                     (last _binds)
                     (drop _exprs (- (length _binds) 2))))
        (map (lambda (b e) (hash-set! data b e))
             _binds
             _exprs)))

    (define/public (set k v)
      (hash-set! data k v)
      v)
    (define/public (get k)
      (hash-ref data k
        (lambda () (unless (null? _outer) (send _outer get k)))))))
