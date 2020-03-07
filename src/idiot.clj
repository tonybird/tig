(ns idiot
  (:require [help :as help]
            [db :as db]
            [parse :as parse]))

(def commands {nil           help/help
               "-h"          help/help
               "--help"      help/help
               "help"        help/help
               "init"        db/init
               "hash-object" db/hash-object
               "cat-file"    db/cat-file
               "write-wtree" help/help
               "commit-tree" help/help})

(defn -main [& args]
  (let [{command :command d-val :d r-val :r} (parse/parse-flags args)
        opts {:d d-val :r r-val}
        sub-command (first command)
        sub-args (rest command)]
    (if (contains? commands sub-command)
      ((get commands sub-command) opts sub-args)
      (println "Error: invalid command"))))