#lang racket

(provide readline)

(require (prefix-in readline: readline/readline))

(require "types.rkt")

(define history-loaded #f)
(define HISTORY-FILE (format "~a/.mal-history" (find-system-path 'home-dir)))

(define (load-history path)
  (with-handlers
    ([exn:fail? (lambda (e) #t)])
    (map
      (lambda (line) (readline:add-history line))
      (string-split
        (port->string (open-input-file path))
        #px"\n"))))

(define (readline prompt)
  (when (not history-loaded)
    (set! history-loaded #t)
    (load-history HISTORY-FILE))
  (let ([line (readline:readline prompt)])
    (if (eq? eof line)
      nil
      (begin
        (readline:add-history line)
        (with-handlers
          ([exn:fail? (lambda (e) #t)])
          (with-output-to-file
            HISTORY-FILE
            (lambda () (printf "~a~n" line))
            #:exists 'append))
        line))))
