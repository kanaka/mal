(defn make-env
  [&opt outer]
  @{:data @{}
    :outer outer})

(defn env-set
  [env sym value]
  (put-in env [:data sym]
              value))

(defn env-find
  [env sym]
  (if (get-in env [:data sym])
    env
    (when-let [outer (get env :outer)]
      (env-find outer sym))))

(defn env-get
  [env sym]
  (if-let [goal-env (env-find env sym)]
    (get-in goal-env [:data sym])
    (error (string (sym :content) " not found." ))))
