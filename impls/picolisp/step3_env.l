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
   (if (= (MAL-type Ast) 'list)
      (if (not (MAL-value Ast))
         Ast
         (let (Ast* (MAL-value Ast)
               A0* (MAL-value (car Ast*))
               A1* (MAL-value (cadr Ast*))
               A2 (caddr Ast*))
            (cond
               ((= A0* 'def!)
                (set> Env A1* (EVAL A2 Env)) )
               ((= A0* 'let*)
                (let Env* (MAL-env Env)
                   (for (Bindings A1* Bindings)
                      (let (Key (MAL-value (pop 'Bindings))
                            Value (EVAL (pop 'Bindings) Env*))
                         (set> Env* Key Value) ) )
                   (EVAL A2 Env*) ) )
               (T (let Value (MAL-value (eval-ast Ast Env))
                     (apply (car Value) (cdr Value)) ) ) ) ) )
      (eval-ast Ast Env) ) )

(de eval-ast (Ast Env)
   (let Value (MAL-value Ast)
      (case (MAL-type Ast)
         (symbol (get> Env Value))
         (list (MAL-list (mapcar '((Form) (EVAL Form Env)) Value)))
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
