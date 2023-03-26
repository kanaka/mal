(define-library (lib util)

(export call-with-input-string call-with-output-string
        str prn debug
        string-intersperse explode
        char->string
        list->alist alist->list alist-ref alist-map
        ->list car-safe cdr-safe contains? last butlast
        identity readline

        ;; HACK: cyclone doesn't have those
        error-object? read-error? error-object-message error-object-irritants)

(import (scheme base))
(import (scheme write))

(begin

(cond-expand
 ;; HACK: cyclone currently implements error the SICP way
 (cyclone
  (define (error-object? x) (and (pair? x) (string? (car x))))
  (define read-error? error-object?)
  (define error-object-message car)
  (define error-object-irritants cdr))
 ;; HACK: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=62464
 (guile
  (define %error-object-irritants error-object-irritants)
  (set! error-object-irritants
        (lambda (ex)
          (or (%error-object-irritants ex) '()))))
 (else))

(define (call-with-input-string string proc)
  (let ((port (open-input-string string)))
    (dynamic-wind
        (lambda () #t)
        (lambda () (proc port))
        (lambda () (close-input-port port)))))

(define (call-with-output-string proc)
  (let ((port (open-output-string)))
    (dynamic-wind
        (lambda () #t)
        (lambda () (proc port) (get-output-string port))
        (lambda () (close-output-port port)))))

(define (str . items)
  (call-with-output-string
   (lambda (port)
     (for-each (lambda (item) (display item port)) items))))

(define (prn . items)
  (for-each (lambda (item) (write item) (display " ")) items)
  (newline))

(define (debug . items)
  (parameterize ((current-output-port (current-error-port)))
    (apply prn items)))

(define (intersperse items sep)
  (let loop ((items items)
             (acc '()))
    (if (null? items)
        (reverse acc)
        (let ((tail (cdr items)))
          (if (null? tail)
              (loop (cdr items) (cons (car items) acc))
              (loop (cdr items) (cons sep (cons (car items) acc))))))))

(define (string-intersperse items sep)
  (apply string-append (intersperse items sep)))

(define (char->string char)
  (list->string (list char)))

(define (explode string)
  (map char->string (string->list string)))

(define (list->alist items)
  (let loop ((items items)
             (acc '()))
    (if (null? items)
        (reverse acc)
        (let ((key (car items)))
          (when (null? (cdr items))
            (error "unbalanced list"))
          (let ((value (cadr items)))
            (loop (cddr items)
                  (cons (cons key value) acc)))))))

(define (alist->list items)
  (let loop ((items items)
             (acc '()))
    (if (null? items)
        (reverse acc)
        (let ((kv (car items)))
          (loop (cdr items)
                (cons (cdr kv) (cons (car kv) acc)))))))

(define (alist-ref key alist . args)
  (let ((test (if (pair? args) (car args) eqv?))
        (default (if (> (length args) 1) (cadr args) #f)))
    (let loop ((items alist))
      (if (pair? items)
          (let ((item (car items)))
            (if (test (car item) key)
                (cdr item)
                (loop (cdr items))))
          default))))

(define (alist-map proc items)
  (map (lambda (item) (proc (car item) (cdr item))) items))

(define (->list items)
  (if (vector? items)
      (vector->list items)
      items))

(define (car-safe x)
  (if (pair? x)
      (car x)
      '()))

(define (cdr-safe x)
  (if (pair? x)
      (cdr x)
      '()))

(define (contains? items test)
  (let loop ((items items))
    (if (pair? items)
        (if (test (car items))
            #t
            (loop (cdr items)))
        #f)))

(define (last items)
  (when (null? items)
    (error "empty argument"))
  (let loop ((items items))
    (let ((tail (cdr items)))
      (if (pair? tail)
          (loop tail)
          (car items)))))

(define (butlast items)
  (when (null? items)
    (error "empty argument"))
  (let loop ((items items)
             (acc '()))
    (let ((tail (cdr items)))
      (if (pair? tail)
          (loop tail (cons (car items) acc))
          (reverse acc)))))

(define (identity x) x)

(define (readline prompt)
  (display prompt)
  (flush-output-port)
  (let ((input (read-line)))
    (if (eof-object? input)
        #f
        input)))

)

)
