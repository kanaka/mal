(defproject mal "0.0.1-SNAPSHOT"
  :description "Make-A-Lisp"

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [net.n01se/clojure-jna "1.0.0"]]

  ;; To run a step with correct readline behavior:
  ;;   lein trampoline with-profile stepX run
  ;; To generate a executable uberjar (in target/) for a step:
  ;;   lein with-profile stepX repl
  :profiles {:step0 {:main mal.step0-repl
                     :uberjar-name "step0_repl.jar"
                     :aot [mal.step0-repl]}
             :step1 {:main mal.step1-read-print
                     :uberjar-name "step1_read_print.jar"
                     :aot [mal.step1-read-print]}
             :step2 {:main mal.step2-eval
                     :uberjar-name "step2_eval.jar"
                     :aot [mal.step2-eval]}
             :step3 {:main mal.step3-env
                     :uberjar-name "step3_env.jar"
                     :aot [mal.step3-env]}
             :step4 {:main mal.step4-if-fn-do
                     :uberjar-name "step4_if_fn_do.jar"
                     :aot [mal.step4-if-fn-do]}
             :step5 {:main mal.step5-tco
                     :uberjar-name "step5_tco.jar"
                     :aot [mal.step5-tco]}
             :step6 {:main mal.step6-file
                     :uberjar-name "step6_file.jar"
                     :aot [mal.step6-file]}
             :step7 {:main mal.step7-quote
                     :uberjar-name "step7_quote.jar"
                     :aot [mal.step7-quote]}
             :step8 {:main mal.step8-macros
                     :uberjar-name "step8_macros.jar"
                     :aot [mal.step8-macros]}
             :step9 {:main mal.step9-try
                     :uberjar-name "step9_try.jar"
                     :aot [mal.step9-try]}
             :stepA {:main mal.stepA-mal
                     :uberjar-name "stepA_mal.jar"
                     :aot [mal.stepA-mal]}})

