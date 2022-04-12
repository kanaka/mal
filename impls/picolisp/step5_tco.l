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
   (catch 'done
      (while t
       (when (and (get> Env 'DEBUG-EVAL)
                  (not (memq (MAL-type @) '(nil false))))
          (prinl "EVAL: " (pr-str Ast T)))

       (case (MAL-type Ast)
        (list
            (let (Ast* (MAL-value Ast)
                  A0* (MAL-value (car Ast*))
                  A1 (cadr Ast*)
                  A1* (MAL-value A1)
                  A2 (caddr Ast*)
                  A3 (cadddr Ast*) )
               (cond
                  ((not Ast*)
                   (throw 'done Ast))
                  ((= A0* 'def!)
                   (throw 'done (set> Env A1* (EVAL A2 Env))) )
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
                     (let (Ast* (mapcar '((Form) (EVAL Form Env)) Ast*)
                           Fn (car Ast*)
                           Args (cdr Ast*) )
                        (if (isa '+MALFn Fn)
                           (throw 'done (apply (MAL-value Fn) Args))
                           (let Env* (MAL-env (get Fn 'env) (get Fn 'params) Args)
                              (setq Ast (get Fn 'ast) Env Env*) ) ) ) ) ) ) )
        (symbol
         (let (Key   (MAL-value Ast)
               Value (get> Env Key))
            (if Value
               (throw 'done Value)
               (throw 'err (MAL-error (MAL-string (pack "'" Key "' not found")))))))
        (vector (throw 'done
          (MAL-vector (mapcar '((Form) (EVAL Form Env)) (MAL-value Ast)))))
        (map (throw 'done
          (MAL-map (mapcar '((Form) (EVAL Form Env)) (MAL-value Ast)))))
        (T (throw 'done Ast))))))

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
