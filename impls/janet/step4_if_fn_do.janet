(import ./reader)
(import ./printer)
(import ./types :as t)
(import ./env :as e)
(import ./core)

(def repl_env
  (let [env (e/make-env)]
    (eachp [k v] core/ns
      (e/env-set env k v))
    env))

(defn READ
  [code-str]
  (reader/read_str code-str))

(var EVAL nil)

(defn eval_ast
  [ast env]
  (cond
    (t/symbol?* ast)
    (e/env-get env ast)
    #
    (t/hash-map?* ast)
    (t/make-hash-map (struct ;(map |(EVAL $0 env)
                                   (kvs (t/get-value ast)))))
    #
    (t/list?* ast)
    (t/make-list (map |(EVAL $0 env)
                      (t/get-value ast)))
    #
    (t/vector?* ast)
    (t/make-vector (map |(EVAL $0 env)
                        (t/get-value ast)))
    #
    ast))

(varfn EVAL
  [ast env]
  (cond
    (not (t/list?* ast))
    (eval_ast ast env)
    #
    (t/empty?* ast)
    ast
    #
    (let [ast-head (first (t/get-value ast))
          head-name (t/get-value ast-head)]
      (case head-name
        "def!"
        (let [def-name (in (t/get-value ast) 1)
              def-val (EVAL (in (t/get-value ast) 2) env)]
          (e/env-set env
                     def-name def-val)
          def-val)
        #
        "let*"
        (let [new-env (e/make-env env)
              bindings (t/get-value (in (t/get-value ast) 1))]
          (each [let-name let-val] (partition 2 bindings)
                (e/env-set new-env
                           let-name (EVAL let-val new-env)))
          (EVAL (in (t/get-value ast) 2) new-env))
        #
        "do"
        (let [do-body-forms (drop 1 (t/get-value ast))
              res-ast (eval_ast (t/make-list do-body-forms) env)]
          (last (t/get-value res-ast)))
        #
        "if"
        (let [cond-res (EVAL (in (t/get-value ast) 1) env)]
          (if (or (t/nil?* cond-res)
                  (t/false?* cond-res))
            (if-let [else-ast (get (t/get-value ast) 3)]
              (EVAL else-ast env)
              t/mal-nil)
            (EVAL (in (t/get-value ast) 2) env)))
        #
        "fn*"
        (let [args (t/get-value (in (t/get-value ast) 1))
              body (in (t/get-value ast) 2)]
          (t/make-function (fn [params]
                             (EVAL body
                                   (e/make-env env args params)))))
        #
        (let [eval-list (t/get-value (eval_ast ast env))
              f (first eval-list)
              args (drop 1 eval-list)]
          ((t/get-value f) args))))))

(defn PRINT
  [ast]
  (printer/pr_str ast true))

(defn rep
  [code-str]
  (PRINT (EVAL (READ code-str) repl_env)))

(rep "(def! not (fn* (a) (if a false true)))")

# getline gives problems
(defn getstdin [prompt buf]
  (file/write stdout prompt)
  (file/flush stdout)
  (file/read stdin :line buf))

(defn handle-error
  [err]
  (cond
    (t/nil?* err)
    (print)
    ##
    (string? err)
    (print err)
    ##
    (print (string "Error: " (PRINT err)))))

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
         (handle-error err))))))
