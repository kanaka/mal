(import ./reader)
(import ./printer)
(import ./types :as t)
(import ./env :as e)

(defn READ
  [code-str]
  (reader/read_str code-str))

(defn arith-fn
  [op]
  (fn [ast-1 ast-2]
    (t/make-number (op (t/get-value ast-1)
                       (t/get-value ast-2)))))

(def repl_env
  (let [env (e/make-env)]
    (e/env-set env (t/make-symbol "+") (arith-fn +))
    (e/env-set env (t/make-symbol "-") (arith-fn -))
    (e/env-set env (t/make-symbol "*") (arith-fn *))
    (e/env-set env (t/make-symbol "/") (arith-fn /))
    env))

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
        (let [eval-list (t/get-value (eval_ast ast env))
              f (first eval-list)
              args (drop 1 eval-list)]
          (apply f args))))))

(defn PRINT
  [value]
  (printer/pr_str value true))

(defn rep
  [code-str]
  (PRINT (EVAL (READ code-str) repl_env)))

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
