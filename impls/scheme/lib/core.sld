(define-library (lib core)

(export ns)

(import (scheme base))
(import (scheme write))
(import (scheme file))
(import (scheme time))
(import (scheme read))
(import (scheme eval))
;; HACK: cyclone doesn't implement environments yet, but its eval
;; behaves as if you were using the repl environment
(cond-expand
 (cyclone)
 (else
  (import (scheme repl))))

(import (lib types))
(import (lib util))
(import (lib printer))
(import (lib reader))

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
      (mal-map-equal? a-value b-value))
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

(define (mal-map-ref key m . default)
  (if (pair? default)
      (alist-ref key m mal-equal? (car default))
      (alist-ref key m mal-equal?)))

(define (mal-map-equal? as bs)
  (if (not (= (length as) (length bs)))
      #f
      (let loop ((as as))
        (if (pair? as)
            (let* ((item (car as))
                   (key (car item))
                   (value (cdr item)))
              (if (mal-equal? (mal-map-ref key bs) value)
                  (loop (cdr as))
                  #f))
            #t))))

(define (mal-map-dissoc m keys)
  (let loop ((items m)
             (acc '()))
    (if (pair? items)
        (let* ((item (car items))
               (key (car item)))
          (if (contains? keys (lambda (x) (mal-equal? key x)))
              (loop (cdr items) acc)
              (loop (cdr items) (cons item acc))))
        (reverse acc))))

(define (mal-map-assoc m kvs)
  (let ((kvs (list->alist kvs)))
    (append kvs (mal-map-dissoc m (map car kvs)))))

(define (map-in-order proc items)
  (let loop ((items items)
             (acc '()))
    (if (null? items)
        (reverse acc)
        (loop (cdr items) (cons (proc (car items)) acc)))))

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

(define (time-ms)
  (* (/ (current-jiffy) (jiffies-per-second)) 1000.0))

(define (->mal-object x)
  (cond
   ((boolean? x) (if x mal-true mal-false))
   ((char? x) (mal-string (char->string x)))
   ((procedure? x) x)
   ((symbol? x) (mal-symbol x))
   ((number? x) (mal-number x))
   ((string? x) (mal-string x))
   ((or (null? x) (pair? x))
    (mal-list (map ->mal-object x)))
   ((vector? x)
    (mal-vector (vector-map ->mal-object x)))
   (else
    (error "unknown type"))))

(define (scm-eval input)
  (call-with-input-string input
    (lambda (port)
      (cond-expand
       (cyclone
        (->mal-object (eval (read port))))
       (else
        (->mal-object (eval (read port) (environment '(scheme base)
                                                     '(scheme write)))))))))

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
    (throw . ,(lambda (x) (raise (cons 'user-error x))))
    (readline . ,(lambda (prompt) (let ((output (readline (mal-value prompt))))
                                    (if output (mal-string output) mal-nil))))
    (time-ms . ,(lambda () (mal-number (time-ms))))
    (scm-eval . ,(lambda (input) (scm-eval (mal-value input))))

    (atom . ,(lambda (x) (mal-atom x)))
    (atom? . ,(lambda (x) (coerce (mal-instance-of? x 'atom))))
    (deref . ,(lambda (atom) (mal-value atom)))
    (reset! . ,(lambda (atom x) (mal-value-set! atom x) x))
    (swap! . ,(lambda (atom fn . args)
                (let* ((fn (if (func? fn) (func-fn fn) fn))
                       (value (apply fn (cons (mal-value atom) args))))
                  (mal-value-set! atom value)
                  value)))

    (cons . ,(lambda (x xs) (mal-list (cons x (->list (mal-value xs))))))
    (concat . ,(lambda args (mal-list (apply append (map (lambda (arg) (->list (mal-value arg))) args)))))
    (vec . ,(lambda (x)
              (case (mal-type x)
                ((vector) x)
                ((list)   (mal-vector (list->vector (mal-value x))))
                (else     (error "seq expects a sequence")))))
    (nth . ,(lambda (x n) (let ((items (->list (mal-value x)))
                                (index (mal-value n)))
                            (if (< index (length items))
                                (list-ref items index)
                                (error (str "Out of range: " index))))))
    (first . ,(lambda (x) (if (eq? x mal-nil)
                              mal-nil
                              (let ((items (->list (mal-value x))))
                                (if (null? items)
                                    mal-nil
                                    (car items))))))
    (rest . ,(lambda (x) (if (eq? x mal-nil)
                             (mal-list '())
                             (let ((items (->list (mal-value x))))
                               (if (null? items)
                                   (mal-list '())
                                   (mal-list (cdr items)))))))
    (conj . ,(lambda (coll . args)
               (let ((items (mal-value coll)))
                 (cond
                  ((vector? items)
                   (mal-vector (vector-append items (list->vector args))))
                  ((list? items)
                   (mal-list (append (reverse args) items)))
                  (else
                   (error "invalid collection type"))))))
    (seq . ,(lambda (x) (if (eq? x mal-nil)
                            mal-nil
                            (let ((value (mal-value x)))
                              (case (mal-type x)
                                ((list)
                                 (if (null? value)
                                     mal-nil
                                     x))
                                ((vector)
                                 (if (zero? (vector-length value))
                                     mal-nil
                                     (mal-list (vector->list value))))
                                ((string)
                                 (if (zero? (string-length value))
                                     mal-nil
                                     (mal-list (map mal-string (explode value)))))
                                (else
                                 (error "invalid collection type")))))))

    (apply . ,(lambda (f . args) (apply (if (func? f) (func-fn f) f)
                                        (if (pair? (cdr args))
                                            (append (butlast args)
                                                    (->list (mal-value (last args))))
                                            (->list (mal-value (car args)))))))
    (map . ,(lambda (f items) (mal-list (map-in-order
                                         (if (func? f) (func-fn f) f)
                                         (->list (mal-value items))))))

    (nil? . ,(lambda (x) (coerce (eq? x mal-nil))))
    (true? . ,(lambda (x) (coerce (eq? x mal-true))))
    (false? . ,(lambda (x) (coerce (eq? x mal-false))))
    (number? . ,(lambda (x) (coerce (mal-instance-of? x 'number))))
    (string? . ,(lambda (x) (coerce (mal-instance-of? x 'string))))
    (symbol? . ,(lambda (x) (coerce (mal-instance-of? x 'symbol))))
    (symbol . ,(lambda (x) (mal-symbol (string->symbol (mal-value x)))))
    (keyword? . ,(lambda (x) (coerce (mal-instance-of? x 'keyword))))
    (keyword . ,(lambda (x) (if (mal-instance-of? x 'keyword)
                                x
                                (mal-keyword (string->symbol (mal-value x))))))
    (vector? . ,(lambda (x) (coerce (mal-instance-of? x 'vector))))
    (vector . ,(lambda args (mal-vector (list->vector args))))
    (map? . ,(lambda (x) (coerce (mal-instance-of? x 'map))))
    (hash-map . ,(lambda args (mal-map (list->alist args))))
    (sequential? . ,(lambda (x) (coerce (and (mal-object? x)
                                             (memq (mal-type x)
                                                   '(list vector))))))
    (fn? . ,(lambda (x) (coerce (or (procedure? x)
                                    (and (func? x) (not (func-macro? x)))))))
    (macro? . ,(lambda (x) (coerce (and (func? x) (func-macro? x)))))

    (assoc . ,(lambda (m . kvs) (mal-map (mal-map-assoc (mal-value m) kvs))))
    (dissoc . ,(lambda (m . keys) (mal-map (mal-map-dissoc (mal-value m) keys))))
    (get . ,(lambda (m key) (mal-map-ref key (mal-value m) mal-nil)))
    (contains? . ,(lambda (m key) (coerce (mal-map-ref key (mal-value m)))))
    (keys . ,(lambda (m) (mal-list (map car (mal-value m)))))
    (vals . ,(lambda (m) (mal-list (map cdr (mal-value m)))))

    (with-meta . ,(lambda (x meta)
                    (cond
                     ((mal-object? x)
                      (make-mal-object (mal-type x) (mal-value x) meta))
                     ((func? x)
                      (let ((func (make-func (func-ast x) (func-params x)
                                             (func-env x) (func-fn x))))
                        (func-macro?-set! func #f)
                        (func-meta-set! func meta)
                        func))
                     (else
                      (error "unsupported type")))))
    (meta . ,(lambda (x) (cond
                          ((mal-object? x)
                           (or (mal-meta x) mal-nil))
                          ((func? x)
                           (or (func-meta x) mal-nil))
                          (else
                           mal-nil))))

    ))

)

)
