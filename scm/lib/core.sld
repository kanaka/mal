(define-library (lib core)

(export ns)

(import (scheme base))
(import (scheme write))

(import (lib types))
(import (lib util))
(import (lib printer))

(begin

(define (coerce x)
  (if x mal-true mal-false))

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

(define ns
  `((+ . ,(lambda (a b) (mal-number (+ (mal-value a) (mal-value b)))))
    (- . ,(lambda (a b) (mal-number (- (mal-value a) (mal-value b)))))
    (* . ,(lambda (a b) (mal-number (* (mal-value a) (mal-value b)))))
    (/ . ,(lambda (a b) (mal-number (/ (mal-value a) (mal-value b)))))

    (list . ,(lambda args (mal-list args)))
    (list? . ,(lambda (x) (coerce (and (mal-object? x) (eq? (mal-type x) 'list)))))
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

    ))

)

)
