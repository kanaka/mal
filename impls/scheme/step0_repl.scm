(import (scheme base))
(import (scheme write))

(define (READ input)
  input)

(define (EVAL input)
  input)

(define (PRINT input)
  input)

(define (rep input)
  (PRINT (EVAL (READ input))))

(define (readline prompt)
  (display prompt)
  (flush-output-port)
  (let ((input (read-line)))
    (if (eof-object? input)
        #f
        input)))

(define (main)
  (let loop ()
    (let ((input (readline "user> ")))
      (when input
        (display (rep input))
        (newline)
        (loop))))
  (newline))

(main)
