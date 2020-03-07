(ns parse
  (:require [clojure.java.io :as io]))

(defn parse-r-flag [[_ dir & rest]]
  (when (or (= dir "-d") (= dir nil))
    (println "Error: the -r switch needs an argument")
    (System/exit 1))
  (when (not (.exists (io/file dir)))
    (println "Error: the directory specified by -r does not exist")
    (System/exit 2))
  {:r dir :command rest})

(defn parse-d-flag [[_ dir & rest]]
  (when (or (= dir "-r") (= dir nil))
    (println "Error: the -d switch needs an argument")
    (System/exit 1))
  {:d dir :command rest})

(defn parse-flags [args]
  (let [top-level-flags {"-r" parse-r-flag "-d" parse-d-flag}]
    (loop [result {:command args :d ".idiot" :r "."} opts top-level-flags]
      (let [flag (first (:command result)) command (:command result)]
        (if (contains? opts flag)
          (recur
           (merge result ((get opts flag) command))
           (dissoc opts (first args)))
          result)))))