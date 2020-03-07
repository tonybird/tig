(ns db
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [help :refer [help]]
            [util :refer [sha1-sum zip unzip]]))

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

(defn- add-header [blob]
  (str "blob " (count blob) \u0000 blob))

(defn- remove-header [header+blob]
  (->> (char 0)
       (str/index-of header+blob)
       (+ 1)
       (subs header+blob)))

(defn- save-blob-to-db [header+blob address opts]
  (let [path (generate-path opts address)]
    (io/make-parents path)
    (io/copy (zip header+blob) (io/file path))
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
                  address (-> file add-header sha1-sum)]
              (cond (nil? file) (println "Error: that file isn't readable")
                    w (save-blob-to-db (add-header file) address opts)
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

(defn write-wtree [opts args]
  (let [cmd (first args)
        h (or (= cmd "-h") (= cmd "--help"))
        r (:r opts)
        d (:d opts)]
    (cond
      h (help '("write-wtree"))
      (not (.exists (io/file (str r "/" d)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (some? cmd) (println "Error: write-wtree accepts no arguments"))))

(defn commit-tree [opts args]
  (let [address (first args)
        h (or (= address "-h") (= address "--help"))
        m (= (second args) "-m")
        m-value (and (>= (count args) 3) (nth args 2))
        p (and (>= (count args) 4) (= "-p" (nth args 3)))
        p-value (and (>= (count args) 5) (nth args 4))
        r (:r opts)
        d (:d opts)]
    (cond
      h (help '("commit-tree"))
      (not (.exists (io/file (str r "/" d)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (nil? address) (println "Error: you must specify a tree address.")
      (not m) (println "Error: you must specify a message.")
      (not m-value) (println "Error: you must specify a message with the -m switch.")
      (and p (not p-value)) (println "Error: you must specify a commit object with the -p switch."))))