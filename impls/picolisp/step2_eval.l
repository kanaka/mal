(de load-relative (Path)
   (load (pack (car (file)) Path)) )

(load-relative "readline.l")
(load-relative "types.l")
(load-relative "reader.l")
(load-relative "printer.l")

(de READ (String)
   (read-str String) )

(def '*ReplEnv
   '((+ . ((A B) (MAL-number (+ (MAL-value A) (MAL-value B)))))
     (- . ((A B) (MAL-number (- (MAL-value A) (MAL-value B)))))
     (* . ((A B) (MAL-number (* (MAL-value A) (MAL-value B)))))
     (/ . ((A B) (MAL-number (/ (MAL-value A) (MAL-value B))))) ) )

(de EVAL (Ast Env)
   ;; (prinl "EVAL: " (pr-str Ast T))
   (let Value (MAL-value Ast)
      (case (MAL-type Ast)
         (symbol
            (if (assoc Value Env)
               (cdr @)
               (throw 'err (MAL-error (MAL-string (pack "'" Value "' not found")))) ) )
         (list
          (if Value
            (let El (mapcar '((Form) (EVAL Form Env)) Value)
                 (apply (car El) (cdr El)))
            Ast))
         (vector (MAL-vector (mapcar '((Form) (EVAL Form Env)) Value)))
         (map (MAL-map (mapcar '((Form) (EVAL Form Env)) Value)))
         (T Ast) ) ) )

(de PRINT (Ast)
   (pr-str Ast T) )

(de rep (String)
   (PRINT (EVAL (READ String) *ReplEnv)) )

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
