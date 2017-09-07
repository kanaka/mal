(define-library (lib core)

(export ns)

(import (scheme base))
(import (scheme write))
(import (scheme file))

(import (lib types))
(import (lib util))
(import (lib printer))
(import (lib reader))

(begin

(define (coerce x)
  (if x mal-true mal-false))

(define (mal-instance-of? x type)
  (and (mal-object? x) (eq? (mal-type x) type)))

(define (->printed-string args print-readably sep)
  (let ((items (map (lambda (arg) (pr-str arg print-readably)) args)))
    (string-intersperse items sep)))

(define (mal-equal? a b)
  (let ((a-type (and (mal-object? a) (mal-type a)))
        (a-value (and (mal-object? a) (mal-value a)))
        (b-type (and (mal-object? b) (mal-type b)))
        (b-value (and (mal-object? b) (mal-value b))))
    (cond
     ((or (not a-type) (not b-type))
      mal-false)
     ((and (memq a-type '(list vector))
           (memq b-type '(list vector)))
      (mal-list-equal? (->list a-value) (->list b-value)))
     ((and (eq? a-type 'map) (eq? b-type 'map))
      (error "TODO"))
     (else
      (and (eq? a-type b-type)
           (equal? a-value b-value))))))

(define (mal-list-equal? as bs)
  (let loop ((as as)
             (bs bs))
    (cond
     ((and (null? as) (null? bs)) #t)
     ((or (null? as) (null? bs)) #f)
     (else
      (if (mal-equal? (car as) (car bs))
          (loop (cdr as) (cdr bs))
          #f)))))

(define (slurp path)
  (call-with-output-string
   (lambda (out)
     (call-with-input-file path
       (lambda (in)
         (let loop ()
           (let ((chunk (read-string 1024 in)))
             (when (not (eof-object? chunk))
               (display chunk out)
               (loop)))))))))

(define ns
  `((+ . ,(lambda (a b) (mal-number (+ (mal-value a) (mal-value b)))))
    (- . ,(lambda (a b) (mal-number (- (mal-value a) (mal-value b)))))
    (* . ,(lambda (a b) (mal-number (* (mal-value a) (mal-value b)))))
    (/ . ,(lambda (a b) (mal-number (/ (mal-value a) (mal-value b)))))

    (list . ,(lambda args (mal-list args)))
    (list? . ,(lambda (x) (coerce (mal-instance-of? x 'list))))
    (empty? . ,(lambda (lis) (coerce (null? (->list (mal-value lis))))))
    (count . ,(lambda (lis) (mal-number
                             (if (eq? lis mal-nil)
                                 0
                                 (length (->list (mal-value lis)))))))

    (< . ,(lambda (a b) (coerce (< (mal-value a) (mal-value b)))))
    (<= . ,(lambda (a b) (coerce (<= (mal-value a) (mal-value b)))))
    (> . ,(lambda (a b) (coerce (> (mal-value a) (mal-value b)))))
    (>= . ,(lambda (a b) (coerce (>= (mal-value a) (mal-value b)))))
    (= . ,(lambda (a b) (coerce (mal-equal? a b))))

    (pr-str . ,(lambda args (mal-string (->printed-string args #t " "))))
    (str . ,(lambda args (mal-string (->printed-string args #f ""))))
    (prn . ,(lambda args
              (display (->printed-string args #t " "))
              (newline)
              mal-nil))
    (println . ,(lambda args
                  (display (->printed-string args #f " "))
                  (newline)
                  mal-nil))

    (read-string . ,(lambda (string) (read-str (mal-value string))))
    (slurp . ,(lambda (path) (mal-string (slurp (mal-value path)))))

    (atom . ,(lambda (x) (mal-atom x)))
    (atom? . ,(lambda (x) (coerce (mal-instance-of? x 'atom))))
    (deref . ,(lambda (atom) (mal-value atom)))
    (reset! . ,(lambda (atom x) (mal-value-set! atom x) x))
    (swap! . ,(lambda (atom fn . args)
                (let* ((fn (if (func? fn) (func-fn fn) fn))
                       (value (apply fn (cons (mal-value atom) args))))
                  (mal-value-set! atom value)
                  value)))

    ))

)

)
