(import ./reader)
(import ./printer)
(import ./types :as t)

(defn READ
  [code-str]
  (reader/read_str code-str))

(defn arith-fn
  [op]
  (fn [ast-1 ast-2]
    (t/make-number (op (t/get-value ast-1)
                       (t/get-value ast-2)))))

(def repl_env
  {(t/make-symbol "+") (arith-fn +)
   (t/make-symbol "-") (arith-fn -)
   (t/make-symbol "*") (arith-fn *)
   (t/make-symbol "/") (arith-fn /)})

(var EVAL nil)

(defn eval_ast
  [ast env]
  (cond
    (t/symbol?* ast)
    (if-let [val (env ast)]
      val
      (error (t/make-string (string "unbound symbol: " (t/get-value ast)))))
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
    (let [eval-list (t/get-value (eval_ast ast env))
          f (first eval-list)
          args (drop 1 eval-list)]
      (apply f args))))

(defn PRINT
  [value]
  (printer/pr_str value true))

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
