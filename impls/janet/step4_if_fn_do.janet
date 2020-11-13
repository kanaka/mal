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

(varfn EVAL
  [ast env]
  (cond
    (not= :list (ast :tag))
    (eval_ast ast env)
    #
    (empty? (ast :content))
    ast
    #
    (let [ast-head (in (ast :content) 0)
          head-name (ast-head :content)]
      (case head-name
        "def!"
        (let [def-name (in (ast :content) 1)
              def-val (EVAL (in (ast :content) 2) env)]
          (env-set env
                   def-name def-val)
          def-val)
        #
        "let*"
        (let [new-env (make-env env)
              bindings ((in (ast :content) 1) :content)]
          (each [let-name let-val] (partition 2 bindings)
                (env-set new-env
                         let-name (EVAL let-val new-env)))
          (EVAL (in (ast :content) 2) new-env))
        #
        "do"
        (let [do-body-forms (drop 1 (ast :content))
              res-ast (eval_ast (make-list do-body-forms) env)]
          (last (res-ast :content)))
        #
        "if"
        (let [cond-res (EVAL (in (ast :content) 1) env)
              cond-type (cond-res :tag)
              cond-val (cond-res :content)]
          (if (or (= cond-type :nil)
                  (and (= cond-type :boolean)
                       (= cond-val "false")))
            (if-let [else-ast (get (ast :content) 3)]
              (EVAL else-ast env)
              (make-nil))
            (EVAL (in (ast :content) 2) env)))
        #
        "fn*"
        (let [args ((in (ast :content) 1) :content)
              body (in (ast :content) 2)]
          (make-function (fn [params]
                           (EVAL body
                                 (make-env env args params)))))
        #
        (let [eval-list ((eval_ast ast env) :content)
              head (first eval-list)
              tail (drop 1 eval-list)]
          ((head :content) tail))
        ))))

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

# getline gives problems
(defn getstdin [prompt buf]
  (file/write stdout prompt)
  (file/flush stdout)
  (file/read stdin :line buf))

(defn main
  [& args]
  (var buf nil)
  (while true
    (set buf @"")
    (getstdin "user> " buf)
    (if (= 0 (length buf))
      (break)
      (try
        (print (rep buf))
        ([err]
         (print err))))))
