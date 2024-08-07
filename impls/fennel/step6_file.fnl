(local printer (require :printer))
(local reader (require :reader))
(local t (require :types))
(local e (require :env))
(local core (require :core))
(local u (require :utils))

(local repl_env
  (let [env (e.make-env)]
    (each [name func (pairs core)]
      (e.env-set env
                 (t.make-symbol name)
                 func))
    env))

(fn READ
  [code-str]
  (reader.read_str code-str))

(fn EVAL
    [ast-param env-param]
    (var ast ast-param)
    (var env env-param)
    (var result nil)
    (while (not result)
      (let [dbgeval (e.env-get env "DEBUG-EVAL")]
        (when (and dbgeval
                   (not (t.nil?* dbgeval))
                   (not (t.false?* dbgeval)))
          (print (.. "EVAL: " (printer.pr_str ast true)))))
      (if (t.symbol?* ast)
          (let [key (t.get-value ast)]
            (set result (or (e.env-get env key)
                        (u.throw* (t.make-string (.. "'" key
                                                   "' not found"))))))
          ;;
          (t.vector?* ast)
          (set result (t.make-vector (u.map (fn [x] (EVAL x env))
                                            (t.get-value ast))))
          ;;
          (t.hash-map?* ast)
          (set result (t.make-hash-map (u.map (fn [x] (EVAL x env))
                                              (t.get-value ast))))
          ;;
          (or (not (t.list?* ast)) (t.empty?* ast))
          (set result ast)
          ;;
          (let [ast-elts (t.get-value ast)
                head-name (t.get-value (. ast-elts 1))]
            ;; XXX: want to check for symbol, but that screws up logic below
            (if (= "def!" head-name)
                (let [def-name (. ast-elts 2)
                      def-val (EVAL (. ast-elts 3) env)]
                  (e.env-set env
                             def-name def-val)
                  (set result def-val))
                ;;
                (= "let*" head-name)
                (let [new-env (e.make-env env)
                      bindings (t.get-value (. ast-elts 2))
                      stop (/ (length bindings) 2)]
                  (for [idx 1 stop]
                       (let [b-name (. bindings (- (* 2 idx) 1))
                             b-val (EVAL (. bindings (* 2 idx)) new-env)]
                         (e.env-set new-env
                                    b-name b-val)))
                  ;; tco
                  (set ast (. ast-elts 3))
                  (set env new-env))
                ;;
                (= "do" head-name)
                (let [most-forms (u.slice ast-elts 2 -2) ;; XXX
                      last-body-form (u.last ast-elts)
                      res-ast (u.map (fn [x] (EVAL x env)) most-forms)]
                  ;; tco
                  (set ast last-body-form))
                ;;
                (= "if" head-name)
                (let [cond-res (EVAL (. ast-elts 2) env)]
                  (if (or (t.nil?* cond-res)
                          (t.false?* cond-res))
                      (let [else-ast (. ast-elts 4)]
                        (if (not else-ast)
                            ;; tco
                            (set result t.mal-nil)
                            (set ast else-ast)))
                      ;; tco
                      (set ast (. ast-elts 3))))
                ;;
                (= "fn*" head-name)
                (let [params (t.get-value (. ast-elts 2))
                      body (. ast-elts 3)]
                  ;; tco
                  (set result
                       (t.make-fn (fn [args]
                                    (EVAL body
                                          (e.make-env env params args)))
                                  body params env)))
                ;;
                (let [f (EVAL (. ast-elts 1) env)
                      args (u.map (fn [x] (EVAL x env)) (u.slice ast-elts 2 -1))
                      body (t.get-ast f)] ;; tco
                    (if body
                        (do
                          (set ast body)
                          (set env (e.make-env (t.get-env f)
                                               (t.get-params f) args)))
                        (set result
                             ((t.get-value f) args))))))))
    result)

(fn PRINT
  [ast]
  (printer.pr_str ast true))

(fn rep
  [code-str]
  (PRINT (EVAL (READ code-str) repl_env)))

(rep "(def! not (fn* (a) (if a false true)))")

(e.env-set repl_env
           (t.make-symbol "eval")
           (t.make-fn
             (fn [asts]
               (when (< (length asts) 1)
                 ;; XXX
                 (error "eval takes 1 arguments"))
               (EVAL (u.first asts) repl_env))))

(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")

(e.env-set repl_env
           (t.make-symbol "*ARGV*")
           (t.make-list (u.map t.make-string (u.slice arg 2))))

(fn handle-error
  [err]
  (if (t.nil?* err)
      (print)
      (= "string" (type err))
      (print err)
      (print (.. "Error: " (PRINT err)))))

(if (<= 1 (length arg))
    (xpcall (fn []
              (rep (.. "(load-file \"" (. arg 1) "\")"))) ;; XXX: escaping?
            handle-error)
    (do
     (var done false)
     (while (not done)
       (io.write "user> ")
       (io.flush)
       (let [input (io.read)]
         (if (not input)
             (set done true)
        (xpcall (fn []
                  (print (rep input)))
                handle-error))))))
;                (fn [exc]
                  ;; (if (t.nil?* exc)
                  ;;     (print)
                  ;;     (= "string" (type exc))
                  ;;     (print exc)
                  ;;     (print (PRINT exc))))))))))
