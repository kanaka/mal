(import (scheme base))
(import (scheme write))

(import (lib util))
(import (lib reader))
(import (lib printer))
(import (lib types))

(define (READ input)
  (read-str input))

(define (EVAL ast)
  ast)

(define (PRINT ast)
  (pr-str ast #t))

(define (rep input)
  (PRINT (EVAL (READ input))))

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
