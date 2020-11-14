(import ./reader :prefix "")
(import ./printer :prefix "")
(import ./types :prefix "")
(import ./env :prefix "")

(defn READ
  [code-str]
  (read_str code-str))

(defn arith-fn
  [op]
  (fn [ast-1 ast-2]
    (make-number (op (ast-1 :content)
                     (ast-2 :content)))))

(def repl_env
  (let [env (make-env)]
    (env-set env (make-symbol "+") (arith-fn +))
    (env-set env (make-symbol "-") (arith-fn -))
    (env-set env (make-symbol "*") (arith-fn *))
    (env-set env (make-symbol "/") (arith-fn /))
    env))

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
    (let [ast-head (first (ast :content))
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
        (let [eval-list ((eval_ast ast env) :content)
              head (first eval-list)
              tail (drop 1 eval-list)]
          (apply head tail))))))

(defn PRINT
  [value]
  (pr_str value true))

(defn rep
  [code-str]
  (let [ds (READ code-str)]
    (when ds
      (PRINT
        (try
          (EVAL ds repl_env)
          ([err]
           (make-exception err)))))))

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
