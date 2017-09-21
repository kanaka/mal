(defn env-new [&optional [outer None] [binds None] [exprs None]]
  {:outer outer})

(defn env-find [env k]
  (if
    (.has_key env k)       env
    (.has_key env ':outer) (env-find (get env ':outer) k)
    True                   None))

(defn env-get [env k]
  (setv e (env-find env k))
  (if-not e
    (raise (Exception (+ "'" k "' not found"))))
  (get e k))

(defn env-set [env k v]
  (assoc env k v)
  v)

