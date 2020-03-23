(ns idiot
  (:require [help :as help]
            [db :as db]
            [parse :as parse]
            [tree :as tree]
            [switch :as switch]))

(def commands {nil           help/help
               "-h"          help/help
               "--help"      help/help
               "help"        help/help
               "init"        db/init
               "hash-object" db/hash-object
               "cat-file"    db/cat-file
               "write-wtree" tree/write-wtree
               "commit-tree" tree/commit-tree
               "rev-parse"   switch/rev-parse
               "switch"      switch/switch
               "branch"      switch/branch
               "commit"      switch/commit})

(defn -main [& args]
  (let [{command :command d-val :db root :root} (parse/parse-flags args)
        opts {:db d-val :root root}
        sub-command (first command)
        sub-args (rest command)]
    (if (contains? commands sub-command)
      ((get commands sub-command) opts sub-args)
      (println "Error: invalid command"))))