(use-modules (rnrs)
             (system foreign))

(define (make-blob-pointer len)
  (bytevector->pointer (make-bytevector len)))

(define pcre-ffi (dynamic-link "libpcre"))

(define %pcre-compile2
  (pointer->procedure '*
                       (dynamic-func "pcre_compile2" pcre-ffi)
                       (list '* int '* '* '* '*)))

(define %pcre-compile
  (pointer->procedure '*
                       (dynamic-func "pcre_compile" pcre-ffi)
                       (list '* int '* '* '*)))

(define %pcre-exec
  (pointer->procedure int
                      (dynamic-func "pcre_exec" pcre-ffi)
                      (list '* '* '* int int int '* int)))

(define %pcre-study
  (pointer->procedure '*
                      (dynamic-func "pcre_study" pcre-ffi)
                      (list '* int '*)))

(define %pcre-get-substring
  (pointer->procedure '*
                      (dynamic-func "pcre_get_substring" pcre-ffi)
                      (list '* '* int int '*)))

(define %pcre-free-study
  (pointer->procedure void
                      (dynamic-func "pcre_free_study" pcre-ffi)
                      (list '*)))

(define %pcre-free-substring
  (pointer->procedure void
                      (dynamic-func "pcre_free_substring" pcre-ffi)
                      (list '*)))

(define-record-type pcre
  (fields
   errptr
   (mutable strptr)
   (mutable ovector)
   (mutable matched)
   (mutable code)
   (mutable extra)))
   
(define (%new-pcre)
  (make-pcre (make-blob-pointer uint64) ; errptr
             #f #f 0 #f #f))

(define* (new-pcre re #:optional (options 0))
  (let ((reptr (string->pointer re))
        ;;(errcodeptr (make-blob-pointer int))
        (erroffset (make-blob-pointer int))
        (tableptr %null-pointer)
        (pcre (%new-pcre)))
    ;; FIXME: add exceptional handling
    (pcre-code-set! pcre
                    (%pcre-compile reptr options (pcre-errptr pcre)
                                   erroffset tableptr))
    pcre))

(define* (pcre-match pcre str #:key (study-options 0) (exec-options 0)
                     (ovecsize 30) (offset 0))
  (let ((extra (%pcre-study (pcre-code pcre) study-options (pcre-errptr pcre)))
        (strptr (string->pointer str))
        (ovector (make-blob-pointer (* int ovecsize))))
    (pcre-matched-set! pcre
                       (%pcre-exec (pcre-code pcre)
                                   extra
                                   strptr
                                   (string-length str)
                                   offset
                                   exec-options
                                   ovector
                                   ovecsize))
    (pcre-ovector-set! pcre ovector)
    (pcre-strptr-set! pcre strptr)
    (%pcre-free-study extra)
    pcre))

(define (pcre-get-substring pcre index)
  (let ((strptr (pcre-strptr pcre))
        (ovector (pcre-ovector pcre))
        (matched (pcre-matched pcre))
        (buf (make-blob-pointer uint64)))
    (%pcre-get-substring strptr ovector matched index buf)
    (let ((ret (pointer->string (dereference-pointer buf))))
      (%pcre-free-substring (dereference-pointer buf))
      ret)))

(define* (pcre-search pcre str #:key (study-options 0) (exec-options 0)
                      (trim string-trim-both))
  (define len (string-length str))
  (let lp((i 0) (ret '()))
    (cond
     ((>= i len) (reverse ret))
     (else
      (pcre-match pcre str #:study-options study-options #:exec-options exec-options #:offset i)
      (let* ((sub (pcre-get-substring pcre 0))
             (sublen (string-length sub)))
        (lp (+ i sublen) (cons (trim sub) ret)))))))
