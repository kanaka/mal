(import ./reader)
(import ./printer)
(import ./types :as t)
(import ./utils :as u)
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

(var DEBUG-EVAL (t/make-symbol "DEBUG-EVAL"))

(varfn EVAL
  [ast env]

    (if-let [dbgeval (e/env-get env DEBUG-EVAL)]
      (if (not (or (t/nil?* dbgeval)
                   (t/false?* dbgeval)))
        (print (string "EVAL: " (printer/pr_str ast true)))))

    (case (t/get-type ast)

    :symbol
    (or (e/env-get env ast)
      (u/throw*
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
            head-name (t/get-value ast-head)]
        (case head-name
          "def!"
          (let [def-name (in (t/get-value ast) 1)
                def-val (EVAL (in (t/get-value ast) 2) env)]
            (e/env-set env
                       def-name def-val)
            def-val)
          ##
          "let*"
          (let [new-env (e/make-env env)
                bindings (t/get-value (in (t/get-value ast) 1))]
            (each [let-name let-val] (partition 2 bindings)
                  (e/env-set new-env
                             let-name (EVAL let-val new-env)))
            (EVAL (in (t/get-value ast) 2) new-env))
          ##
          "do"
          (let [most-do-body-forms (slice (t/get-value ast) 1 -2)
                last-body-form (last (t/get-value ast))]
            (each x most-do-body-forms (EVAL x env))
            (EVAL last-body-form env))
          ##
          "if"
          (let [cond-res (EVAL (in (t/get-value ast) 1) env)]
            (if (or (t/nil?* cond-res)
                    (t/false?* cond-res))
              (if-let [else-ast (get (t/get-value ast) 3)]
                (EVAL else-ast env)
                t/mal-nil)
              (EVAL (in (t/get-value ast) 2) env)))
          ##
          "fn*"
          (let [params (t/get-value (in (t/get-value ast) 1))
                body (in (t/get-value ast) 2)]
              (t/make-function (fn [args]
                                 (EVAL body
                                       (e/make-env env params args)))))
          ##
          (let [f (EVAL ast-head env)
                raw-args (drop 1 (t/get-value ast))
                args (map |(EVAL $0 env) raw-args)]
            ((t/get-value f) args)))))

    # Neither a list, map, symbol or vector.
    ast))

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
