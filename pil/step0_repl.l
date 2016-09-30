(de load-relative (Path)
   (load (pack (car (file)) Path)) )

(load-relative "readline.l")

(de READ (String)
   String)

(de EVAL (Ast)
   Ast)

(de PRINT (Ast)
   Ast)

(de rep (String)
   (PRINT (EVAL (READ String))) )

(load-history ".mal_history")

(use Eof
   (until Eof
      (let Input (readline "user> ")
         (if (=0 Input)
            (setq Eof T)
            (prinl (rep Input)) ) ) ) )

(prinl)
(bye)
