(class +Reader)
# tokens
(dm T (Tokens)
   (=: tokens Tokens) )

(dm next> ()
   (pop (:: tokens)) )

(dm peek> ()
   (car (: tokens)) )

(de read-str (String)
   (let (Tokens (tokenizer String)
         Reader (new '(+Reader) Tokens) )
      (read-form Reader) ) )

(de tokenizer (String)
   # [\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)
   (let (Special " []{}()'\"`,;" )
      (make
         (for (Chars (chop String) Chars)
            (let Char (pop 'Chars)
               (cond
                  ((or (sp? Char) (= Char ","))
                   # do nothing, whitespace
                   )
                  ((and (= Char "~") (= (car Chars) "@"))
                   (link "~@")
                   (pop 'Chars) ) # remove @ token
                  ((index Char (chop "[]{}()'`~^\@"))
                   (link Char) )
                  ((= Char "\"")
                   (link
                      (pack
                         (make
                            (link Char) # HACK
                            (use Done
                               (while (and Chars (not Done))
                                  (let Char (pop 'Chars)
                                     (cond
                                        ((= Char "\\")
                                         (if Chars
                                            (let Char (pop 'Chars)
                                               (if (= Char "n")
                                                  (link "\n")
                                                  (link Char) ) )
                                            (throw 'err (MAL-error (MAL-string "expected '\"', got EOF"))) ) )
                                        ((<> Char "\"")
                                         (link Char) )
                                        ((= Char "\"")
                                         (setq Done T) ) ) ) )
                               (unless Done
                                  (throw 'err (MAL-error (MAL-string "expected '\"', got EOF"))) ) ) ) ) ) )
                  ((= Char ";")
                   (while (and Chars (<> Char "\n"))
                      (setq Char (pop 'Chars)) ) )
                  ((and (not (index Char (chop Special))) (not (sp? Char)))
                   (link
                      (pack
                         (make
                            (link Char)
                            (let Char (car Chars)
                               (while (and Chars (not (index Char (chop Special))) (not (sp? Char)))
                                  (link (pop 'Chars))
                                  (setq Char (car Chars)) ) ) ) ) ) ) ) ) ) ) ) )

(de read-form (Reader)
   (case (peek> Reader)
      ("'" (read-macro Reader 'quote))
      ("`" (read-macro Reader 'quasiquote))
      ("~" (read-macro Reader 'unquote))
      ("~@" (read-macro Reader 'splice-unquote))
      ("@" (read-macro Reader 'deref))
      ("\^" (read-meta Reader))
      ("(" (read-list Reader 'list ")"))
      ("[" (read-list Reader 'vector "]"))
      ("{" (read-list Reader 'map "}"))
      (T (read-atom Reader)) ) )

(de read-macro (Reader symbol)
   (next> Reader) # pop reader macro token
   (MAL-list (list (MAL-symbol symbol) (read-form Reader))) )

(de read-meta (Reader)
   (next> Reader) # pop reader macro token
   (let Form (read-form Reader)
      (MAL-list (list (MAL-symbol 'with-meta) (read-form Reader) Form) ) ) )

(de read-list (Reader Type Ender)
   (next> Reader) # pop list start
   (new (list (case Type
                 (list '+MALList)
                 (vector '+MALVector)
                 (map '+MALMap) ) )
      (make
         (use Done
            (while (not Done)
               (let Token (peek> Reader)
                  (cond
                     ((= Token Ender)
                      (next> Reader) # pop list end
                      (setq Done T) )
                     ((not Token)
                      (let Msg (pack "expected '" Ender "', got EOF")
                         (throw 'err (MAL-error (MAL-string Msg))) ) )
                     (T (link (read-form Reader))) ) ) ) ) ) ) )

(de read-atom (Reader)
   (let (Token (next> Reader)
         Chars (chop Token))
      (cond
         ((= Token "true")
          *MAL-true)
         ((= Token "false")
          *MAL-false)
         ((= Token "nil")
          *MAL-nil)
         ((format Token)
          (MAL-number @) )
         ((= (car Chars) "\"")
          (MAL-string (pack (cdr Chars))) )
         ((= (car Chars) ":")
          (MAL-keyword (intern (pack (cdr Chars)))) )
         ((not Token)
          (throw 'err (MAL-error (MAL-string "end of token stream"))) )
         (T (MAL-symbol (intern Token))) ) ) )
