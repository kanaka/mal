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

(var EVAL nil)

(defn eval_ast
  [ast env]
  (case (ast :tag)
    :symbol
    (env-get env ast)
    #
    :hash-map
    (make-hash-map (map |(EVAL $0 env)
                        (ast :content)))
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

(varfn EVAL
  [ast-param env-param]
  (var ast ast-param)
  (var env env-param)
  (label result
    (while true
      (cond
        (not= :list (ast :tag))
        (return result (eval_ast ast env))
        ##
        (empty? (ast :content))
        (return result ast)
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
            (return result (core/quasiquote* (in (ast :content) 1)))
            ##
            "quasiquote"
            ## tco
            (set ast (core/quasiquote* (in (ast :content) 1)))
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
                        ((f :content) args))))))))))

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

(rep ```
  (def! load-file
        (fn* (fpath)
          (eval
            (read-string (str "(do "
                                (slurp fpath) "\n"
                                "nil)")))))
```)

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
