(ns mal.readline)

(def readline (.-readline (js/require "../src/mal/node_readline.js")))
