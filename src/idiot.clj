(ns idiot
  (:require [help :as help]
            [db :as db]))

(def commands {nil           help/help
               "-h"          help/help
               "--help"      help/help
               "help"        help/help
               "init"        db/init
               "hash-object" db/hash-object
               "cat-file"    db/cat-file})

(defn -main [& args]
  (if (contains? commands (first args))
    ((get commands (first args)) (rest args))
    (println "Error: invalid command")))