(define-library (lib printer)

(export pr-str)

(import (scheme base))
(import (scheme write))

(import (lib util))
(import (lib types))

(begin

(define (pr-str ast print-readably)
  (cond
   ((procedure? ast)
    "#<fn>")
   ((func? ast)
    "#<func>")
   (else
    (if (procedure? ast)
        "#<fn>"
        (let* ((type (and (mal-object? ast) (mal-type ast)))
               (value (and (mal-object? ast) (mal-value ast))))
          (case type
            ((true) "true")
            ((false) "false")
            ((nil) "nil")
            ((number) (number->string value))
            ((string) (call-with-output-string
                       (lambda (port)
                         (if print-readably
                             (begin
                               (display #\" port)
                               (string-for-each
                                (lambda (char)
                                  (case char
                                    ((#\\) (display "\\\\" port))
                                    ((#\") (display "\\\"" port))
                                    ((#\newline) (display "\\n" port))
                                    (else (display char port))))
                                value)
                               (display #\" port))
                             (display value port)))))
            ((keyword) (string-append ":" (symbol->string value)))
            ((symbol) (symbol->string value))
            ((list) (pr-list value "(" ")" print-readably))
            ((vector) (pr-list (vector->list value) "[" "]" print-readably))
            ((map) (pr-list (alist->list value) "{" "}" print-readably))
            ((atom) (string-append "(atom " (pr-str value print-readably) ")"))
            (else (error "unknown type"))))))))

(define (pr-list items starter ender print-readably)
  (call-with-output-string
   (lambda (port)
     (display starter port)
     (let ((reprs (map (lambda (item) (pr-str item print-readably)) items)))
       (display (string-intersperse reprs " ") port))
     (display ender port))))

)

)
