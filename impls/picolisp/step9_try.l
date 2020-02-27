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

(de is-pair (Ast)
   (and (memq (MAL-type Ast) '(list vector)) (MAL-value Ast) T) )

(de quasiquote (Ast)
   (if (not (is-pair Ast))
      (MAL-list (list (MAL-symbol 'quote) Ast))
      (let A (MAL-value Ast)
         (cond
            ((= (MAL-value (car A)) 'unquote)
             (cadr A) )
            ((and (is-pair (car A))
                (= (MAL-value (car (MAL-value (car A)))) 'splice-unquote) )
             (MAL-list (list (MAL-symbol 'concat)
                          (cadr (MAL-value (car A)))
                          (quasiquote (MAL-list (cdr A))) ) ) )
            (T
               (MAL-list (list (MAL-symbol 'cons)
                            (quasiquote (car A))
                            (quasiquote (MAL-list (cdr A))) ) ) ) ) ) ) )

(de is-macro-call (Ast Env)
   (when (= (MAL-type Ast) 'list)
      (let A0 (car (MAL-value Ast))
         (when (= (MAL-type A0) 'symbol)
            (let Value (find> Env (MAL-value A0))
               (and (isa '+Func Value) (get Value 'is-macro) T) ) ) ) ) )

(de macroexpand (Ast Env)
   (while (is-macro-call Ast Env)
      (let (Ast* (MAL-value Ast)
            Macro (get (find> Env (MAL-value (car Ast*))) 'fn)
            Args (cdr Ast*) )
         (setq Ast (apply (MAL-value Macro) Args)) ) )
   Ast )

(de EVAL (Ast Env)
   (catch 'done
      (while t
         (when (not (= (MAL-type Ast) 'list))
            (throw 'done (eval-ast Ast Env)) )
         (setq Ast (macroexpand Ast Env))
         (when (or (not (= (MAL-type Ast) 'list)) (not (MAL-value Ast)))
            (throw 'done (eval-ast Ast Env)) )
         (let (Ast* (MAL-value Ast)
               A0* (MAL-value (car Ast*))
               A1 (cadr Ast*)
               A1* (MAL-value A1)
               A2 (caddr Ast*)
               A3 (cadddr Ast*) )
            (cond
               ((= A0* 'def!)
                (throw 'done (set> Env A1* (EVAL A2 Env))) )
               ((= A0* 'quote)
                (throw 'done A1) )
               ((= A0* 'quasiquote)
                (setq Ast (quasiquote A1)) ) # TCO
               ((= A0* 'defmacro!)
                (let Form (EVAL A2 Env)
                   (put Form 'is-macro T)
                   (throw 'done (set> Env A1* Form)) ) )
               ((= A0* 'macroexpand)
                (throw 'done (macroexpand A1 Env)) )
               ((= A0* 'try*)
                (let Result (catch 'err (throw 'done (EVAL A1 Env)))
                   (if (isa '+MALError Result)
                      (let A (MAL-value A2)
                         (if (and (= (MAL-type A2) 'list)
                                (= (MAL-value (car A)) 'catch*) )
                            (let (Bind (MAL-value (cadr A))
                                  Exc (MAL-value Result)
                                  Form (caddr A)
                                  Env* (MAL-env Env (list Bind) (list Exc)) )
                               (throw 'done (EVAL Form Env*)) )
                            (throw 'err Result) ) )
                      (throw 'done Result) ) ) )
               ((= A0* 'let*)
                (let Env* (MAL-env Env)
                   (for (Bindings A1* Bindings)
                      (let (Key (MAL-value (pop 'Bindings))
                            Value (EVAL (pop 'Bindings) Env*) )
                         (set> Env* Key Value) ) )
                   (setq Env Env* Ast A2) ) ) # TCO
               ((= A0* 'do)
                (mapc '((Form) (EVAL Form Env)) (head -1 (cdr Ast*)))
                (setq Ast (last Ast*)) ) # TCO
               ((= A0* 'if)
                (if (not (memq (MAL-type (EVAL A1 Env)) '(nil false)))
                   (setq Ast A2) # TCO
                   (if A3
                      (setq Ast A3) # TCO
                      (throw 'done *MAL-nil) ) ) )
               ((= A0* 'fn*)
                (let (Binds (mapcar MAL-value A1*)
                      Body A2
                      Fn (MAL-fn
                            (curry (Env Binds Body) @
                               (let Env* (MAL-env Env Binds (rest))
                                  (EVAL Body Env*) ) ) ) )
                   (throw 'done (MAL-func Env Body Binds Fn)) ) )
               (T
                  (let (Ast* (MAL-value (eval-ast Ast Env))
                        Fn (car Ast*)
                        Args (cdr Ast*) )
                     (if (isa '+MALFn Fn)
                        (throw 'done (apply (MAL-value Fn) Args))
                        (let Env* (MAL-env (get Fn 'env) (get Fn 'params) Args)
                           (setq Ast (get Fn 'ast) Env Env*) ) ) ) ) ) ) ) ) )

(de eval-ast (Ast Env)
   (let Value (MAL-value Ast)
      (case (MAL-type Ast)
         (symbol (get> Env Value))
         (list (MAL-list (mapcar '((Form) (EVAL Form Env)) Value)))
         (vector (MAL-vector (mapcar '((Form) (EVAL Form Env)) Value)))
         (map (MAL-map (mapcar '((Form) (EVAL Form Env)) Value)))
         (T Ast) ) ) )

(set> *ReplEnv 'eval (MAL-fn (curry (*ReplEnv) (Form) (EVAL Form *ReplEnv))))
(set> *ReplEnv '*ARGV* (MAL-list (mapcar MAL-string (cdr (argv)))))

(de PRINT (Ast)
   (pr-str Ast T) )

(de rep (String)
   (PRINT (EVAL (READ String) *ReplEnv)) )

(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")
(rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))")

(load-history ".mal_history")

(if (argv)
   (rep (pack "(load-file \"" (car (argv)) "\")"))
   (use Input
      (until (=0 (setq Input (readline "user> ")))
         (let Output (catch 'err (rep Input))
            (if (isa '+MALError Output)
               (let Message (MAL-value Output)
                  (unless (= (MAL-value Message) "end of token stream")
                     (prinl "[error] " (pr-str Message)) ) )
               (prinl Output) ) ) ) ) )

(prinl)
(bye)
