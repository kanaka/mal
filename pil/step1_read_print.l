(de load-relative (Path)
   (load (pack (car (file)) Path)) )

(load-relative "readline.l")
(load-relative "types.l")
(load-relative "reader.l")
(load-relative "printer.l")

(de READ (String)
   (read-str String) )

(de EVAL (Ast)
   Ast)

(de PRINT (Ast)
   (pr-str Ast T) )

(de rep (String)
   (PRINT (EVAL (READ String))) )

(load-history ".mal_history")

(use Eof
   (until Eof
      (let Input (readline "user> ")
         (if (=0 Input)
            (setq Eof T)
            (let Output (catch 'err (rep Input))
                (if (isa '+MALError Output)
                   (let Message (MAL-value Output)
                      (unless (= (MAL-value Message) "end of token stream")
                         (prinl "[error] " (pr-str Message)) ) )
                   (prinl Output) ) ) ) ) ) )

(prinl)
(bye)
