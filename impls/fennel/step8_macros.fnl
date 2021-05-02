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

(fn is_macro_call
  [ast env]
  (when (and (t.list?* ast)
             (not (t.empty?* ast)))
    (let [head-ast (. (t.get-value ast) 1)]
      (when (and (t.symbol?* head-ast)
                 (e.env-find env head-ast))
        (let [target-ast (e.env-get env head-ast)]
          (t.macro?* target-ast))))))

(fn macroexpand
  [ast env]
  (var ast-var ast)
  (while (is_macro_call ast-var env)
    (let [inner-asts (t.get-value ast-var)
          head-ast (. inner-asts 1)
          macro-fn (t.get-value (e.env-get env head-ast))
          args (u.slice inner-asts 2 -1)]
      (set ast-var (macro-fn args))))
  ast-var)

;; forward declaration
(var EVAL 1)

(fn eval_ast
  [ast env]
  (if (t.symbol?* ast)
      (e.env-get env ast)
      ;;
      (t.list?* ast)
      (t.make-list (u.map (fn [elt-ast]
                            (EVAL elt-ast env))
                          (t.get-value ast)))
      ;;
      (t.vector?* ast)
      (t.make-vector (u.map (fn [elt-ast]
                              (EVAL elt-ast env))
                            (t.get-value ast)))
      ;;
      (t.hash-map?* ast)
      (t.make-hash-map (u.map (fn [elt-ast]
                                (EVAL elt-ast env))
                              (t.get-value ast)))
      ;;
      ast))

(fn starts-with
  [ast name]
  (when (and (t.list?* ast)
             (not (t.empty?* ast)))
    (let [head-ast (. (t.get-value ast) 1)]
      (and (t.symbol?* head-ast)
           (= name (t.get-value head-ast))))))

(var quasiquote* nil)

(fn qq-iter
  [ast]
  (if (t.empty?* ast)
      (t.make-list [])
      (let [ast-value (t.get-value ast)
            elt (. ast-value 1)
            acc (qq-iter (t.make-list (u.slice ast-value 2 -1)))]
        (if (starts-with elt "splice-unquote")
            (t.make-list [(t.make-symbol "concat")
                          (. (t.get-value elt) 2)
                          acc])
            (t.make-list [(t.make-symbol "cons")
                          (quasiquote* elt)
                          acc])))))

(set quasiquote*
  (fn [ast]
    (if (starts-with ast "unquote")
        (. (t.get-value ast) 2)
        ;;
        (t.list?* ast)
        (qq-iter ast)
        ;;
        (t.vector?* ast)
        (t.make-list [(t.make-symbol "vec") (qq-iter ast)])
        ;;
        (or (t.symbol?* ast)
            (t.hash-map?* ast))
        (t.make-list [(t.make-symbol "quote") ast])
        ;;
        ast)))

(set EVAL
  (fn [ast-param env-param]
    (var ast ast-param)
    (var env env-param)
    (var result nil)
    (while (not result)
      (if (not (t.list?* ast))
          (set result (eval_ast ast env))
          (do
           (set ast (macroexpand ast env))
           (if (not (t.list?* ast))
               (set result (eval_ast ast env))
               (if (t.empty?* ast)
                   (set result ast)
                   (let [ast-elts (t.get-value ast)
                         head-name (t.get-value (. ast-elts 1))]
                     ;; XXX: want to check for symbol, but...
                     (if (= "def!" head-name)
                         (let [def-name (. ast-elts 2)
                               def-val (EVAL (. ast-elts 3) env)]
                           (e.env-set env
                                      def-name def-val)
                           (set result def-val))
                         ;;
                         (= "defmacro!" head-name)
                         (let [def-name (. ast-elts 2)
                               def-val (EVAL (. ast-elts 3) env)
                               macro-ast (t.macrofy def-val)]
                           (e.env-set env
                                      def-name macro-ast)
                           (set result macro-ast))
                         ;;
                         (= "macroexpand" head-name)
                         (set result (macroexpand (. ast-elts 2) env))
                         ;;
                         (= "let*" head-name)
                         (let [new-env (e.make-env env)
                               bindings (t.get-value (. ast-elts 2))
                               stop (/ (length bindings) 2)]
                           (for [idx 1 stop]
                                (let [b-name
                                      (. bindings (- (* 2 idx) 1))
                                      b-val
                                      (EVAL (. bindings (* 2 idx)) new-env)]
                                  (e.env-set new-env
                                             b-name b-val)))
                           ;; tco
                           (set ast (. ast-elts 3))
                           (set env new-env))
                         ;;
                         (= "quote" head-name)
                         ;; tco
                         (set result (. ast-elts 2))
                         ;;
                         (= "quasiquoteexpand" head-name)
                         ;; tco
                         (set result (quasiquote* (. ast-elts 2)))
                         ;;
                         (= "quasiquote" head-name)
                         ;; tco
                         (set ast (quasiquote* (. ast-elts 2)))
                         ;;
                         (= "do" head-name)
                         (let [most-forms (u.slice ast-elts 2 -2) ;; XXX
                               last-body-form (u.last ast-elts)
                               res-ast (eval_ast
                                        (t.make-list most-forms) env)]
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
                                (t.make-fn
                                 (fn [args]
                                   (EVAL body
                                         (e.make-env env params args)))
                                 body params env false)))
                         ;;
                         (let [eval-list (t.get-value (eval_ast ast env))
                               f (. eval-list 1)
                               args (u.slice eval-list 2 -1)]
                           (let [body (t.get-ast f)] ;; tco
                             (if body
                                 (do
                                  (set ast body)
                                  (set env
                                       (e.make-env (t.get-env f)
                                                   (t.get-params f)
                                                   args)))
                                 (set result
                                      ((t.get-value f) args))))))))))))
    result))

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

(rep
 (.. "(def! load-file "
     "  (fn* (f) "
     "    (eval "
     "      (read-string "
     "        (str \"(do \" (slurp f) \"\nnil)\")))))"))

(rep
 (.. "(defmacro! cond "
     "  (fn* (& xs) "
     "    (if (> (count xs) 0) "
     "        (list 'if (first xs) "
     "                  (if (> (count xs) 1) "
     "                      (nth xs 1) "
     "                      (throw \"odd number of forms to cond\")) "
     "                  (cons 'cond (rest (rest xs)))))))"))

(e.env-set repl_env
           (t.make-symbol "*ARGV*")
           (t.make-list (u.map t.make-string (u.slice arg 2 -1))))

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
