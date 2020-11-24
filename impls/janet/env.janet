(import ./types :as t)
(import ./utils :as u)

(defn make-env
  [&opt outer binds exprs]
  (default binds [])
  (default exprs [])
  (def n-binds (length binds))
  (var found-amp false)
  (var idx 0)
  (while (and (not found-amp)
              (< idx n-binds))
    (def c-bind (in binds idx))
    (when (= (t/get-value c-bind) "&")
      (set found-amp true)
      (break))
    (++ idx))
  (def new-binds
    (if found-amp
      (array/concat (array ;(slice binds 0 idx))
                    (in binds (inc idx)))
      binds))
  (def new-exprs
    (if found-amp
      (array/concat (array ;(slice exprs 0 idx))
                    (array (t/make-list (slice exprs idx))))
      exprs))
  # XXX: would length mismatches of new-binds / new-exprs ever be an issue?
  @{:data (zipcoll new-binds new-exprs)
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
    (u/throw*
      (t/make-string
        (string "'" (t/get-value sym) "'" " not found" )))))
