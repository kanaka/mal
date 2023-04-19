(de load-relative (Path)
   (load (pack (car (file)) Path)) )

(load-relative "readline.l")
(load-relative "types.l")
(load-relative "reader.l")
(load-relative "printer.l")
(load-relative "env.l")

(de READ (String)
   (read-str String) )

(def '*ReplEnv (MAL-env NIL))
(set> *ReplEnv '+ '((A B) (MAL-number (+ (MAL-value A) (MAL-value B)))))
(set> *ReplEnv '- '((A B) (MAL-number (- (MAL-value A) (MAL-value B)))))
(set> *ReplEnv '* '((A B) (MAL-number (* (MAL-value A) (MAL-value B)))))
(set> *ReplEnv '/ '((A B) (MAL-number (/ (MAL-value A) (MAL-value B)))))

(de EVAL (Ast Env)
   (when (and (get> Env 'DEBUG-EVAL)
              (not (memq (MAL-type @) '(nil false))))
      (prinl "EVAL: " (pr-str Ast T)))

   (case (MAL-type Ast)
      (list
         (let (Ast* (MAL-value Ast)
               A0* (MAL-value (car Ast*))
               A1* (MAL-value (cadr Ast*))
               A2 (caddr Ast*))
            (cond
               ((not Ast*)
                Ast)
               ((= A0* 'def!)
                (set> Env A1* (EVAL A2 Env)) )
               ((= A0* 'let*)
                (let Env* (MAL-env Env)
                   (for (Bindings A1* Bindings)
                      (let (Key (MAL-value (pop 'Bindings))
                            Value (EVAL (pop 'Bindings) Env*))
                         (set> Env* Key Value) ) )
                   (EVAL A2 Env*) ) )
               (T (let Value (mapcar '((Form) (EVAL Form Env)) Ast*)
                     (apply (car Value) (cdr Value)) ) ) ) ) )
      (symbol
       (let (Key (MAL-value Ast))
          (or (get> Env Key)
              (throw 'err (MAL-error (MAL-string (pack "'" Key "' not found")))))))
      (vector (MAL-vector (mapcar '((Form) (EVAL Form Env)) (MAL-value Ast))))
      (map (MAL-map (mapcar '((Form) (EVAL Form Env)) (MAL-value Ast))))
      (T Ast)))

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
