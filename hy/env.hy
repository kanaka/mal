(import [hy.models [HySymbol :as Sym]])

(defn env-new [&optional [outer None] [binds []] [exprs []]]
  (setv env {:outer outer})
  (while binds
    (if
      (= (Sym "&") (first binds))
      (do (assoc env (nth binds 1) (tuple exprs)) (break))

      True
      (do (assoc env (first binds) (first exprs))
          (setv binds (list (rest binds))
                exprs (list (rest exprs))))))
  env)

(defn env-find [env k]
  (if
    (.has_key env k)  env
    (get env ':outer) (env-find (get env ':outer) k)
    True              None))

(defn env-get [env k]
  (setv e (env-find env k))
  (if-not e
    (raise (Exception (+ "'" k "' not found"))))
  (get e k))

(defn env-set [env k v]
  (assoc env k v)
  v)

