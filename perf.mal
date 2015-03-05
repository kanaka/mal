(defmacro! time
  (fn* (exp)
    `(let* (start_FIXME (time-ms)
            ret_FIXME ~exp)
      (do
        (prn (str "Elapsed time: " (- (time-ms) start_FIXME) " msecs"))
        ret_FIXME))))

(def! run-fn-for*
  (fn* [fn max-ms acc-ms iters]
    (let* [start (time-ms)
           _ (fn)
           elapsed (- (time-ms) start)
           new-iters (+ 1 iters)
           new-acc-ms (+ acc-ms elapsed)]
      ;(do (prn "here:" new-acc-ms "/" max-ms "iters:" new-iters) )
      (if (>= new-acc-ms max-ms)
        (/ (* max-ms iters) new-acc-ms)
        (run-fn-for* fn max-ms new-acc-ms new-iters)))))

(def! run-fn-for
  (fn* [fn max-secs]
    (do
      ;; Warm it up first
      (run-fn-for* fn 1000 0 0)
      ;; Now do the test
      (/ (run-fn-for* fn (* 1000 max-secs) 0 0) 3))))
