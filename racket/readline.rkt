#lang racket

(provide readline)

(require "types.rkt")

(define (readline prompt)
  (_printf "~a" prompt)
  (let ([line (read-line (current-input-port) 'any)])
    (if (eq? eof line)
      nil
      line)))

      

