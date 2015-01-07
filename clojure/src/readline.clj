(ns readline
    (:require [clojure.string :refer [split]]
              [net.n01se.clojure-jna :as jna]))

(defonce history-loaded (atom nil))
(def HISTORY-FILE (str (System/getProperty "user.home") "/.mal-history"))

;;
;; Uncomment one of the following readline libraries
;;

;; editline (BSD)
#_
(do
  (def readline-call (jna/to-fn String edit/readline))
  (def add-history (jna/to-fn Void edit/add_history))
  (def load-history #(doseq [line (split (slurp %) #"\n")]
                       (jna/invoke Void edit/add_history line))))

;; GNU Readline (GPL)
;; WARNING: distributing your code with GNU readline enabled means you
;; must release your program as GPL
;#_
(do 
  (def readline-call (jna/to-fn String readline/readline))
  (def add-history (jna/to-fn Void readline/add_history))
  (def load-history (jna/to-fn Integer readline/read_history)))

(defn readline [prompt & [lib]]
  (when (not @history-loaded)
    (reset! history-loaded true)
    (load-history HISTORY-FILE))
  (let [line (readline-call prompt)]
    (when line
      (add-history line)
      (spit HISTORY-FILE (str line "\n") :append true))
    line))
