(defproject mal "0.0.1-SNAPSHOT"
  :description "Make-A-Lisp"

  :dependencies [[org.clojure/clojure "1.8.0-RC4"]
                 [org.clojure/tools.reader "0.8.3"]
                 [net.n01se/clojure-jna "1.0.0"]]

  ;; To run a step with correct readline behavior:
  ;;   lein trampoline with-profile stepX run
  ;; To load step in repl:
  ;;   lein with-profile +stepX repl
  :profiles {:step0 {:main step0-repl}
             :step1 {:main step1-read-print}
             :step2 {:main step2-eval}
             :step3 {:main step3-env}
             :step4 {:main step4-if-fn-do}
             :step5 {:main step5-tco}
             :step6 {:main step6-file}
             :step7 {:main step7-quote}
             :step8 {:main step8-macros}
             :step9 {:main step9-try}
             :stepA {:main stepA-mal}}

   :main stepA-more)

