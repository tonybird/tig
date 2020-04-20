(ns parse
  (:require [clojure.java.io :as io]))

(defn parse-r-flag [[_ dir & rest]]
  (cond
    (or (= dir "-d") (= dir nil)) (do (println "Error: the -r switch needs an argument") {:root nil :command nil})
    (not (.exists (io/file dir))) (do (println "Error: the directory specified by -r does not exist") {:root nil :command nil})
    :else {:root dir :command rest}))

(defn parse-d-flag [[_ dir & rest]]
  (cond
    (or (= dir "-r") (= dir nil)) (do (println "Error: the -d switch needs an argument") {:db dir :command rest})
    :else {:db dir :command rest}))

(defn parse-flags [args]
  (let [top-level-flags {"-r" parse-r-flag "-d" parse-d-flag}]
    (loop [result {:command args :db ".idiot" :root "."} opts top-level-flags]
      (let [flag (first (:command result)) command (:command result)]
        (if (contains? opts flag)
          (recur
           (merge result ((get opts flag) command))
           (dissoc opts (first args)))
          result)))))

