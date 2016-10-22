(de load-relative (Path)
   (load (pack (car (file)) Path)) )

(load-relative "readline.l")
(load-relative "types.l")
(load-relative "reader.l")
(load-relative "printer.l")
(load-relative "env.l")
(load-relative "func.l")
(load-relative "core.l")

(de READ (String)
   (read-str String) )

(def '*ReplEnv (MAL-env NIL))
(for Bind *Ns (set> *ReplEnv (car Bind) (cdr Bind)))

(de EVAL (Ast Env)
   (if (= (MAL-type Ast) 'list)
      (if (not (MAL-value Ast))
         Ast
         (let (Ast* (MAL-value Ast)
               A0* (MAL-value (car Ast*))
               A1 (cadr Ast*)
               A1* (MAL-value A1)
               A2 (caddr Ast*)
               A3 (cadddr Ast*) )
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
               ((= A0* 'do)
                (for Form (cdr Ast*)
                   (EVAL Form Env) ) )
               ((= A0* 'if)
                (if (not (memq (MAL-type (EVAL A1 Env)) '(nil false)))
                   (EVAL A2 Env)
                   (if A3
                      (EVAL A3 Env)
                      *MAL-nil ) ) )
               ((= A0* 'fn*)
                (let (Binds (mapcar MAL-value A1*)
                      Body A2)
                   (MAL-fn
                      (curry (Env Binds Body) @
                         (let Env* (MAL-env Env Binds (rest))
                            (EVAL Body Env*) ) ) ) ) )
               (T
                  (let (Ast* (MAL-value (eval-ast Ast Env))
                        Fn (MAL-value (car Ast*))
                        Args (cdr Ast*))
                     (apply Fn Args) ) ) ) ) )
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

(rep "(def! not (fn* (a) (if a false true)))")

(load-history ".mal_history")

(use Input
   (until (=0 (setq Input (readline "user> ")))
      (let Output (catch 'err (rep Input))
         (if (isa '+MALError Output)
            (let Message (MAL-value Output)
               (unless (= (MAL-value Message) "end of token stream")
                  (prinl "[error] " (pr-str Message)) ) )
            (prinl Output) ) ) ) )

(prinl)
(bye)
