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

(defn EVAL
  [ast env]

  # (print (string "EVAL: " (printer/pr_str ast true)))

    (case (t/get-type ast)

    :symbol
    (or (env ast)
      (error
        (t/make-string
          (string "'" (t/get-value ast) "'" " not found" ))))

    :hash-map
    (t/make-hash-map (struct ;(map |(EVAL $0 env)
                                   (kvs (t/get-value ast)))))

    :vector
    (t/make-vector (map |(EVAL $0 env)
                        (t/get-value ast)))

    :list
    (if (t/empty?* ast)
      ast
      (let [ast-head (in (t/get-value ast) 0)
                f (EVAL ast-head env)
                raw-args (drop 1 (t/get-value ast))
                args (map |(EVAL $0 env) raw-args)]
        (apply f args)))

    # Neither a list, map, symbol or vector.
    ast))

(defn PRINT
  [ast]
  (printer/pr_str ast true))

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
