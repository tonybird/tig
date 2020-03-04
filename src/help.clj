(ns help)

(def docs {nil           "main"
           "-h"          "help"
           "--help"      "help"
           "help"        "help"
           "init"        "init"
           "hash-object" "hash-object"
           "cat-file"    "cat-file"})

(defn help [args]
  (let [cmd (first args)]
    (if (contains? docs cmd)
      (println (slurp (str "./docs/" (get docs cmd) ".txt")))
      (println "Error: invalid command"))))