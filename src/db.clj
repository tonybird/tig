(ns db
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [help :refer [help]]
            [util :refer [sha1-sum zip unzip]]))

(defn init [args]
  (let [cmd (first args)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("init"))
      (some? cmd) (println "Error: init accepts no arguments")
      (.exists (io/file ".git")) (println "Error: .git directory already exists")
      :else (do (io/make-parents ".git/objects/foo")
                (println "Initialized empty Idiot repository in .git directory")))))

(defn generate-path [address]
  (let [dir (subs address 0 2)
        file (subs address 2)]
    (str ".git/objects/" dir "/" file)))

(defn add-header [blob]
  (str "blob " (count blob) \u0000 blob))

(defn remove-header [header+blob]
  (->> (char 0)
       (str/index-of header+blob)
       (+ 1)
       (subs header+blob)))

(defn save-blob-to-db [header+blob address]
  (let [path (generate-path address)]
    (io/make-parents path)
    (io/copy (zip header+blob) (io/file path))
    (println address)))

(defn hash-object [args]
  (let [h (or (= (first args) "-h") (= (first args) "--help"))
        w (= (first args) "-w")
        filename (if w (second args) (first args))]
    (cond
      h (help '("hash-object"))
      (not (.exists (io/file ".git"))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (nil? filename) (println "Error: you must specify a file.")
      :else (let [file (try (slurp filename) (catch Exception _))
                  address (-> file add-header sha1-sum)]
              (cond (nil? file) (println "Error: that file isn't readable")
                    w (save-blob-to-db (add-header file) address)
                    :else (println address))))))

(defn cat-file [args]
  (let [h (or (= (first args) "-h") (= (first args) "--help"))
        p (= (first args) "-p")]
    (cond
      h (help '("cat-file"))
      (not (.exists (io/file ".git"))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (not p) (println "Error: the -p switch is required")
      (nil? (second args)) (println "Error: you must specify an address")
      (-> args second generate-path io/file .exists not) (println "Error: that address doesn't exist")
      :else (-> args second generate-path io/file io/input-stream unzip remove-header print))))