(import ./reader :prefix "")
(import ./printer :prefix "")
(import ./types :prefix "")

(defn READ
  [code-str]
  (read_str code-str))

(defn arith-fn
  [op]
  (fn [ast-1 ast-2]
    {:tag :number
     :content (op (ast-1 :content)
                  (ast-2 :content))}))

(def repl_env
  {"+" (arith-fn +)
   "-" (arith-fn -)
   "*" (arith-fn *)
   "/" (arith-fn /)})

(var EVAL nil)

(defn eval_ast
  [ast env]
  (case (ast :tag)
    :symbol
    (let [name (ast :content)
          val (env name)]
      (if val
        val
        (error (make-string (string "unbound symbol: " name)))))
    #
    :hash-map
    {:tag :hash-map
     :content (struct ;(map |(EVAL $0 env)
                            (kvs (ast :content))))}
    #
    :list
    {:tag :list
     :content (map |(EVAL $0 env)
                   (ast :content))}
    #
    :vector
    {:tag :vector
     :content (map |(EVAL $0 env)
                   (ast :content))}
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
    (let [eval-list ((eval_ast ast env) :content)
          head (first eval-list)
          tail (drop 1 eval-list)]
      (apply head tail))))

(defn PRINT
  [value]
  (pr_str value true))

(defn rep
  [code-str]
  (let [ds (READ code-str)]
    (when ds
      (PRINT
        (EVAL ds repl_env)))))

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
