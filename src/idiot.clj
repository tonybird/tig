(ns idiot
  ;(:refer-clojure :exclude [parse-opts])
  ;:require [clojure.tools.cli :refer [parse-opts]])
  (:require [help :as help]
            [db :as db])
  )

(def commands {nil           help/help
               "-h"          help/help
               "--help"      help/help
               "help"        help/help
               "init"        db/init
               "hash-object" db/hash-object
               "cat-file"    db/cat-file
               "write-wtree" help/help
               "commit-tree" help/help})

;(def cli-options
;  [["-r" nil "Directory"
;    :default "."]
;   ["-d" nil "Database"
;    :default ".git"]
;   ["-h" "--help"]])

;; r only: take second args
;; d only: take second args
;; r then d: take second and fourth
;; d then r: take second and fourth

;(defn seq-contains? [col key]
;  (let [seq col]
;  (some #{key} seq)))

(defn -main [& args]
  (let [r-first? (= (nth args 0) "-r")
        d-first? (= (nth args 0) "-d")
        r-third? (and (> (count args) 2) (= (nth args 2) "-r"))
        d-third? (and (> (count args) 2) (= (nth args 2) "-d"))
        both? (or (and r-first? d-third?) (and d-first? r-third?))
        r (cond
            r-first? (nth args 1)
            r-third? (nth args 3)
            :else ".")
        d (cond
            d-first? (nth args 1)
            d-third? (nth args 3)
            :else ".idiot")
        opts {:r r, :d d}
        cmd (cond
              both? (nth args 4)
              (or r-first? d-first?) (nth args 2)
              :else (first args))
        cmd-rest (cond
                   both? (nthrest args 5)
                   (or r-first? d-first?) (nthrest args 3)
                   :else (rest args))]
  (if (contains? commands cmd)
    ((get commands cmd) opts cmd-rest)
    (println "Error: invalid command"))))