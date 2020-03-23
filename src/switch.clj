(ns switch
  (:require [clojure.java.io :as io]
            [help :refer [help]]
            [tree :refer [tree]]))

(defn rev-parse [opts args]
  (let [cmd (first args)
        root (:root opts)
        db (:db opts)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("rev-parse"))
      (empty? args) (println "Error: you must specify a branch name.")
      (> (count args) 1) (println "Error: you must specify a branch name and nothing else.")
      (not (.exists (io/file (str root "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (or (= cmd "HEAD") (= cmd "@")) (println "print commit address"))))

(defn switch [opts args]
  (let [cmd (first args)
        c (= cmd "-c")
        root (:root opts)
        db (:db opts)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("switch"))
      (empty? args) (println "Error: you must specify a branch name.")
      (and (> (count args) 1) (not c)) (println "Error: you may only specify one branch name.")
      (not (.exists (io/file (str root "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)"))))

(defn branch [opts args]
  (let [cmd (first args)
        d (= cmd "-c")
        root (:root opts)
        db (:db opts)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("branch"))
      (and d (= 1 (count args))) (println "Error: you must specify a branch name.")
      (not= (count args) 2) (println "Error: invalid arguments.")
      (not (.exists (io/file (str root "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)"))))

(defn commit [opts args]
  (let [cmd (first args)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("commit"))
      :else (tree/commit-tree opts args))))