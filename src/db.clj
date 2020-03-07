(ns db
  (:require [clojure.java.io :as io]
            [help :refer [help]]
            [util :refer [add-header remove-header sha1-sum zip unzip]]))

(defn init [opts args]
  (let [cmd (first args)
        r (:r opts)
        d (:d opts)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("init"))
      (some? cmd) (println "Error: init accepts no arguments")
      (.exists (io/file (str r "/" d))) (println "Error:" d "directory already exists")
      :else (do (io/make-parents (str r "/" d "/objects/foo"))
                (println "Initialized empty Idiot repository in" d "directory")))))

(defn- split-path [address]
  (let [dir (subs address 0 2)
        file (subs address 2)]
    (str dir "/" file)))

(defn- generate-path [opts address]
  (let [r (:r opts)
        d (:d opts)]
    (str r "/" d "/objects/" (split-path address))))

(defn save-to-db [header+object address opts]
  (let [path (generate-path opts address)]
    (io/make-parents path)
    (io/copy (zip header+object) (io/file path))
    (println address)))

(defn hash-object [opts args]
  (let [h (or (= (first args) "-h") (= (first args) "--help"))
        w (= (first args) "-w")
        filename (if w (second args) (first args))
        r (:r opts)
        d (:d opts)]
    (cond
      h (help '("hash-object"))
      (not (.exists (io/file (str r "/" d)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (nil? filename) (println "Error: you must specify a file.")
      :else (let [file (try (slurp (str r "/" filename)) (catch Exception _))
                  address (->> file (add-header "blob") sha1-sum)]
              (cond (nil? file) (println "Error: that file isn't readable")
                    w (save-to-db (add-header "blob" file) address opts)
                    :else (println address))))))

(defn cat-file [opts args]
  (let [h (or (= (first args) "-h") (= (first args) "--help"))
        p (= (first args) "-p")
        t (= (first args) "-t")
        r (:r opts)
        d (:d opts)]
    (cond
      h (help '("cat-file"))
      (not (.exists (io/file (str r "/" d)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (not p) (not t)) (println "Error: the -p or -t switch is required")
      (nil? (second args)) (println "Error: you must specify an address")
      (->> args second (generate-path opts) io/file .exists not) (println "Error: that address doesn't exist")
      :else (->> args second (generate-path opts) io/file io/input-stream unzip remove-header print))))