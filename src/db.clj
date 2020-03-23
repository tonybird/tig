(ns db
  (:require [clojure.java.io :as io]
            [help :refer [help]]
            [util :refer [add-header remove-header sha1-sum zip unzip]]))

(defn init [opts args]
  (let [cmd (first args)
        root (:root opts)
        db (:db opts)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("init"))
      (some? cmd) (println "Error: init accepts no arguments")
      (.exists (io/file (str root "/" db))) (println "Error:" db "directory already exists")
      :else (do (io/make-parents (str root "/" db "/objects/foo"))
                (println "Initialized empty Idiot repository in" db "directory")))))

(defn- split-path [address]
  (let [dir (subs address 0 2)
        file (subs address 2)]
    (str dir "/" file)))

(defn- generate-path [opts address]
  (let [root (:root opts)
        db (:db opts)]
    (str root "/" db "/objects/" (split-path address))))

(defn address-exists? [opts address]
  (let [path (generate-path opts address)]
    (.exists (io/file path))))

(defn save-to-db [header+object address opts]
  (let [path (generate-path opts address)]
    (io/make-parents path)
    (io/copy (zip header+object) (io/file path))
    address))

(defn hash-object [opts args]
  (let [h (or (= (first args) "-h") (= (first args) "--help"))
        w (= (first args) "-w")
        filename (if w (second args) (first args))
        root (:root opts)
        db (:db opts)]
    (cond
      h (help '("hash-object"))
      (not (.exists (io/file (str root "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (nil? filename) (println "Error: you must specify a file.")
      :else (let [file (try (slurp (str root "/" filename)) (catch Exception _))
                  address (->> file (add-header "blob") sha1-sum)]
              (cond (nil? file) (println "Error: that file isn't readable")
                    w (println (save-to-db (add-header "blob" file) address opts))
                    :else (println address))))))

(defn get-object [opts address]
  (->> address (generate-path opts) io/file io/input-stream unzip))

(defn cat-file [opts args]
  (let [h (or (= (first args) "-h") (= (first args) "--help"))
        p (= (first args) "-p")
        t (= (first args) "-t")
        root (:root opts)
        db (:db opts)]
    (cond
      h (help '("cat-file"))
      (not (.exists (io/file (str root "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (not p) (not t)) (println "Error: the -p or -t switch is required")
      (nil? (second args)) (println "Error: you must specify an address")
      (->> args second (generate-path opts) io/file .exists not) (println "Error: that address doesn't exist")
      :else (->> args second (generate-path opts) io/file io/input-stream unzip util/bytes->str remove-header print))))