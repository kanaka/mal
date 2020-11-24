(local t (require :types))
(local u (require :utils))

(fn make-env
  [outer binds exprs]
  (local tbl {})
  (when binds
    (local n-binds (length binds))
    (var found-amp false)
    (var i 1)
    (while (and (not found-amp)
                (<= i n-binds))
      (local c-bind (. binds i))
      (if (= (t.get-value c-bind) "&")
          (set found-amp true)
          (set i (+ i 1))))
    (if (not found-amp)
        (for [j 1 n-binds]
          (tset tbl
                (t.get-value (. binds j))
                (. exprs j)))
        (do ; houston, there was an ampersand
          (for [j 1 (- i 1)] ; things before &
            (tset tbl
                  (t.get-value (. binds j))
                  (. exprs j)))
          (tset tbl ; after &, put things in a list
                (t.get-value (. binds (+ i 1)))
                (t.make-list (u.slice exprs i -1))))))
  {:outer outer
   :data tbl})

(fn env-set
  [env sym-ast val-ast]
  (tset (. env :data)
        (t.get-value sym-ast)
        val-ast)
  env)

(fn env-find
  [env sym-ast]
  (let [inner-env (. env :data)
        val-ast (. inner-env (t.get-value sym-ast))]
    (if val-ast
        env
        (let [outer (. env :outer)]
          (when outer
            (env-find outer sym-ast))))))

(fn env-get
  [env sym-ast]
  (let [target-env (env-find env sym-ast)]
    (if target-env
        (. (. target-env :data)
           (t.get-value sym-ast))
        (u.throw*
         (t.make-string (.. "'" (t.get-value sym-ast) "'"
                            " not found"))))))

(comment

 (local test-env (make-env {}))

 (env-set test-env
          (t.make-symbol "fun")
          (t.make-number 1))

 (env-find test-env (t.make-symbol "fun"))

 (env-get test-env (t.make-symbol "fun"))

 (local test-env-2 (make-env nil))

 (env-set test-env-2
          (t.make-symbol "smile")
          (t.make-keyword ":yay"))

 (env-find test-env-2 (t.make-symbol "smile"))

 (env-get test-env-2 (t.make-symbol "smile"))

 (local test-env-3 (make-env nil))

 (env-set test-env-3
          (t.make-symbol "+")
          (fn [ast-1 ast-2]
            (t.make-number (+ (t.get-value ast-1)
                              (t.get-value ast-2)))))

 (env-find test-env-3 (t.make-symbol "+"))

 (env-get test-env-3 (t.make-symbol "+"))

 )

{:make-env make-env
 :env-set env-set
 :env-find env-find
 :env-get env-get}
