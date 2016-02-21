(ns env)

(defn env [& [outer binds exprs]]
  ;;(prn "env" binds exprs)
  ;; (when (not= (count binds) (count exprs))
  ;;  (throw (Exception. "Arity mistmatch in env call")))
  (atom
    (loop [env {:outer outer}
           b binds
           e exprs]
      (cond
        (= nil b)
        env

        (= '& (first b))
        (assoc env (nth b 1) e)

        :else
        (recur (assoc env (first b) (first e)) (next b) (rest e))))))

(defn env-find [env k]
  (cond
    (contains? @env k) env
    (:outer @env) (env-find (:outer @env) k)
    :else nil))

(defn env-get [env k]
  (let [e (env-find env k)]
    (when-not e
      (throw (Exception. (str "'" k "' not found"))))
    (get @e k)))

(defn env-set [env k v]
  (swap! env assoc k v)
  v)
