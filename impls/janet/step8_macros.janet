(import ./reader)
(import ./printer :prefix "")
(import ./types :prefix "")
(import ./env :prefix "")
(import ./core)

(def repl_env
  (let [env (make-env)]
    (eachp [k v] core/ns
      (env-set env k v))
    env))

(defn READ
  [code-str]
  (reader/read_str code-str))

(defn is_macro_call
  [ast env]
  (when (and (core/list?* ast)
             (not (core/empty?* ast)))
    (when-let [head-ast (in (ast :content) 0)]
      (when (= :symbol (head-ast :tag))
        (when (env-find env head-ast)
          (let [target-ast (env-get env head-ast)]
            (and (= :function (target-ast :tag))
                 (target-ast :is-macro))))))))

(defn macroexpand
  [ast env]
  (var ast-var ast)
  (while (is_macro_call ast-var env)
    (let [inner-asts (ast-var :content)
          head-ast (in inner-asts 0)
          macro-fn ((env-get env head-ast) :content)
          args (drop 1 inner-asts)]
      (set ast-var (macro-fn args))))
  ast-var)

(var EVAL nil)

(defn eval_ast
  [ast env]
  (case (ast :tag)
    :symbol
    (env-get env ast)
    #
    :hash-map
    (make-hash-map (struct ;(map |(EVAL $0 env)
                                 (kvs (ast :content)))))
    #
    :list
    (make-list (map |(EVAL $0 env)
                    (ast :content)))
    #
    :vector
    (make-vector (map |(EVAL $0 env)
                      (ast :content)))
    #
    ast))

(defn starts-with
  [ast name]
  (when (and (core/list?* ast)
             (not (core/empty?* ast)))
    (let [head-ast (in (ast :content) 0)]
      (and (= :symbol (head-ast :tag))
           (= name (head-ast :content))))))

(var quasiquote* nil)

(defn qq-iter
  [ast]
  (if (core/empty?* ast)
    (make-list ())
    (let [elt (in (ast :content) 0)
          acc (qq-iter (make-list (slice (ast :content) 1)))]
      (if (starts-with elt "splice-unquote")
        (make-list [(make-symbol "concat")
                    (in (elt :content) 1)
                    acc])
        (make-list [(make-symbol "cons")
                    (quasiquote* elt)
                    acc])))))

(varfn quasiquote*
  [ast]
  (cond
    (starts-with ast "unquote")
    (in (ast :content) 1)
    ##
    (core/list?* ast)
    (qq-iter ast)
    ##
    (core/vector?* ast)
    (make-list [(make-symbol "vec") (qq-iter ast)])
    ##
    (or (= :symbol (ast :tag))
        (= :hash-map (ast :tag)))
    (make-list [(make-symbol "quote") ast])
    ##
    ast))

(varfn EVAL
  [ast-param env-param]
  (var ast ast-param)
  (var env env-param)
  (label result
    (while true
      (when (not= :list (ast :tag))
        (return result (eval_ast ast env)))
      ##
      (set ast (macroexpand ast env))
      ##
      (when (not= :list (ast :tag))
        (return result (eval_ast ast env)))
      ##
      (when (empty? (ast :content))
        (return result ast))
      ##
      (let [ast-head (in (ast :content) 0)
            head-name (ast-head :content)]
        (case head-name
          "def!"
          (let [def-name (in (ast :content) 1)
                def-val (EVAL (in (ast :content) 2) env)]
            (env-set env
                     def-name def-val)
            (return result def-val))
          ##
          "defmacro!"
          (let [def-name (in (ast :content) 1)
                def-val (EVAL (in (ast :content) 2) env)
                macro-ast (make-function (def-val :content)
                                         (def-val :meta)
                                         true
                                         nil nil
                                         (def-val :env))]
            (env-set env
                     def-name macro-ast)
            (return result macro-ast))
          ##
          "macroexpand"
          (return result (macroexpand (in (ast :content) 1) env))
          ##
          "let*"
          (let [new-env (make-env env)
                bindings ((in (ast :content) 1) :content)]
            (each [let-name let-val] (partition 2 bindings)
                  (env-set new-env
                           let-name (EVAL let-val new-env)))
            ## tco
            (set ast (in (ast :content) 2))
            (set env new-env))
          ##
          "quote"
          (return result (in (ast :content) 1))
          ##
          "quasiquoteexpand"
          ## tco
          (return result (quasiquote* (in (ast :content) 1)))
          ##
          "quasiquote"
          ## tco
          (set ast (quasiquote* (in (ast :content) 1)))
          ##
          "do"
          (let [most-do-body-forms (slice (ast :content) 1 -2)
                last-body-form (last (ast :content))
                res-ast (eval_ast (make-list most-do-body-forms) env)]
            ## tco
            (set ast last-body-form))
          ##
          "if"
          (let [cond-res (EVAL (in (ast :content) 1) env)
                cond-type (cond-res :tag)
                cond-val (cond-res :content)]
            (if (or (= cond-type :nil)
                    (and (= cond-type :boolean)
                         (= cond-val "false")))
              (if-let [else-ast (get (ast :content) 3)]
                ## tco
                (set ast else-ast)
                (return result (make-nil)))
              ## tco
              (set ast (in (ast :content) 2))))
          ##
          "fn*"
          (let [params ((in (ast :content) 1) :content)
                body (in (ast :content) 2)]
            ## tco
            (return result
              (make-function (fn [args]
                               (EVAL body
                                 (make-env env params args)))
                             nil false
                             body params env)))
          ##
          (let [eval-list ((eval_ast ast env) :content)
                f (first eval-list)
                args (drop 1 eval-list)]
            (if-let [body (f :ast)] ## tco
              (do
                (set ast body)
                (set env (make-env (f :env) (f :params) args)))
              (return result
                ((f :content) args)))))))))

(defn PRINT
  [ast]
  (pr_str ast true))

(defn rep
  [code-str]
  (let [ds (READ code-str)]
    (when ds
      (PRINT
        (EVAL ds repl_env)))))

(rep "(def! not (fn* (a) (if a false true)))")

(env-set repl_env
         (make-symbol "eval")
         (make-function (fn [asts]
                          (EVAL (in asts 0) repl_env))))

(rep ``
  (def! load-file
        (fn* (fpath)
          (eval
            (read-string (str "(do "
                                (slurp fpath) "\n"
                                "nil)")))))
``)

(rep ``
  (defmacro! cond
    (fn* (& xs)
      (if (> (count xs) 0)
        (list 'if
              (first xs)
              (if (> (count xs) 1)
                  (nth xs 1)
                  (throw "odd number of forms to cond"))
              (cons 'cond (rest (rest xs)))))))
``)

# getline gives problems
(defn getstdin [prompt buf]
  (file/write stdout prompt)
  (file/flush stdout)
  (file/read stdin :line buf))

(defn main
  [& args]
  (let [args-len (length args)
        argv (if (<= 2 args-len)
               (drop 2 args)
               ())]
    (env-set repl_env
             (make-symbol "*ARGV*")
             (make-list (map make-string argv)))
    (if (< 1 args-len)
      (rep
        (string "(load-file \"" (in args 1) "\")")) # XXX: escaping?
      (do
        (var buf nil)
        (while true
          (set buf @"")
          (getstdin "user> " buf)
          (if (= 0 (length buf))
            (break)
            (try
              (print (rep buf))
              ([err]
               (print err)))))))))
