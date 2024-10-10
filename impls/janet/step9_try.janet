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

(defn starts-with
  [ast name]
  (when (and (t/list?* ast)
             (not (t/empty?* ast)))
    (let [head-ast (in (t/get-value ast) 0)]
      (and (t/symbol?* head-ast)
           (= name (t/get-value head-ast))))))

(var quasiquote* nil)

(defn qq-iter
  [ast]
  (if (t/empty?* ast)
    (t/make-list ())
    (let [elt (in (t/get-value ast) 0)
          acc (qq-iter (t/make-list (slice (t/get-value ast) 1)))]
      (if (starts-with elt "splice-unquote")
        (t/make-list [(t/make-symbol "concat")
                      (in (t/get-value elt) 1)
                      acc])
        (t/make-list [(t/make-symbol "cons")
                      (quasiquote* elt)
                      acc])))))

(varfn quasiquote*
  [ast]
  (cond
    (starts-with ast "unquote")
    (in (t/get-value ast) 1)
    ##
    (t/list?* ast)
    (qq-iter ast)
    ##
    (t/vector?* ast)
    (t/make-list [(t/make-symbol "vec") (qq-iter ast)])
    ##
    (or (t/symbol?* ast)
        (t/hash-map?* ast))
    (t/make-list [(t/make-symbol "quote") ast])
    ##
    ast))

(var DEBUG-EVAL (t/make-symbol "DEBUG-EVAL"))

(varfn EVAL
  [ast-param env-param]
  (var ast ast-param)
  (var env env-param)
  (label result
    (while true

    (if-let [dbgeval (e/env-get env DEBUG-EVAL)]
      (if (not (or (t/nil?* dbgeval)
                   (t/false?* dbgeval)))
        (print (string "EVAL: " (printer/pr_str ast true)))))

    (case (t/get-type ast)

    :symbol
    (if-let [value (e/env-get env ast)]
      (return result value)
      (u/throw*
        (t/make-string
          (string "'" (t/get-value ast) "'" " not found" ))))

    :hash-map
    (return result
      (t/make-hash-map (struct ;(map |(EVAL $0 env)
                                     (kvs (t/get-value ast))))))

    :vector
    (return result
      (t/make-vector (map |(EVAL $0 env)
                          (t/get-value ast))))

    :list
    (if (t/empty?* ast)
      (return result ast)
      (let [ast-head (in (t/get-value ast) 0)
            head-name (t/get-value ast-head)]
        (case head-name
          "def!"
          (let [def-name (in (t/get-value ast) 1)
                def-val (EVAL (in (t/get-value ast) 2) env)]
            (e/env-set env
                       def-name def-val)
            (return result def-val))
          ##
          "defmacro!"
          (let [def-name (in (t/get-value ast) 1)
                def-val (EVAL (in (t/get-value ast) 2) env)
                macro-ast (t/macrofy def-val)]
            (e/env-set env
                       def-name macro-ast)
            (return result macro-ast))
          ##
          "let*"
          (let [new-env (e/make-env env)
                bindings (t/get-value (in (t/get-value ast) 1))]
            (each [let-name let-val] (partition 2 bindings)
                  (e/env-set new-env
                             let-name (EVAL let-val new-env)))
            ## tco
            (set ast (in (t/get-value ast) 2))
            (set env new-env))
          ##
          "quote"
          (return result (in (t/get-value ast) 1))
          ##
          "quasiquote"
          ## tco
          (set ast (quasiquote* (in (t/get-value ast) 1)))
          ##
          "try*"
          (let [res
                (try
                  (EVAL (in (t/get-value ast) 1) env)
                  ([err]
                   (if-let [maybe-catch-ast (get (t/get-value ast) 2)]
                     (if (starts-with maybe-catch-ast "catch*")
                       (let [catch-asts (t/get-value maybe-catch-ast)]
                         (if (>= (length catch-asts) 2)
                           (let [catch-sym-ast (in catch-asts 1)
                                 catch-body-ast (in catch-asts 2)]
                             (EVAL catch-body-ast (e/make-env env
                                                              [catch-sym-ast]
                                                              [err])))
                           (u/throw*
                             (t/make-string
                               "catch* requires at least 2 arguments"))))
                       (u/throw*
                         (t/make-string
                           "Expected catch* form")))
                     # XXX: is this appropriate?  show error message?
                     (u/throw* err))))]
            (return result res))
          ##
          "do"
          (let [most-do-body-forms (slice (t/get-value ast) 1 -2)
                last-body-form (last (t/get-value ast))]
            (each x most-do-body-forms (EVAL x env))
            ## tco
            (set ast last-body-form))
          ##
          "if"
          (let [cond-res (EVAL (in (t/get-value ast) 1) env)]
            (if (or (t/nil?* cond-res)
                    (t/false?* cond-res))
              (if-let [else-ast (get (t/get-value ast) 3)]
                ## tco
                (set ast else-ast)
                (return result t/mal-nil))
              ## tco
              (set ast (in (t/get-value ast) 2))))
          ##
          "fn*"
          (let [params (t/get-value (in (t/get-value ast) 1))
                body (in (t/get-value ast) 2)]
            ## tco
            (return result
              (t/make-function (fn [args]
                                 (EVAL body
                                   (e/make-env env params args)))
                               nil false
                               body params env)))
          ##
          (let [f (EVAL ast-head env)
                raw-args (drop 1 (t/get-value ast))]
          (if (t/macro?* f)
          (set ast ((t/get-value f) raw-args))
          (let [args (map |(EVAL $0 env) raw-args)]
            (if-let [body (t/get-ast f)] ## tco
              (do
                (set ast body)
                (set env (e/make-env (t/get-env f) (t/get-params f) args)))
              (return result
                ((t/get-value f) args)))))))))

    # Neither a list, map, symbol or vector.
    (return result ast)))))

(defn PRINT
  [ast]
  (printer/pr_str ast true))

(defn rep
  [code-str]
  (PRINT (EVAL (READ code-str) repl_env)))

(rep "(def! not (fn* (a) (if a false true)))")

(e/env-set repl_env
           (t/make-symbol "eval")
           (t/make-function (fn [asts]
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
  (let [args-len (length args)
        argv (if (<= 2 args-len)
               (drop 2 args)
               ())]
    (e/env-set repl_env
               (t/make-symbol "*ARGV*")
               (t/make-list (map t/make-string argv)))
    (if (< 1 args-len)
      (try
        (rep
          (string "(load-file \"" (in args 1) "\")")) # XXX: escaping?
        ([err]
         (handle-error err)))
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
               (handle-error err)))))))))
