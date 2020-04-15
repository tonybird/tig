(ns db
  (:require [clojure.java.io :as io]
            [help :refer [help]]
            [util :refer [add-header remove-header sha1-sum zip unzip]]))

(defn init [opts args]
  (let [cmd (first args)
        root (:root opts)
        db (:db opts)
        dir (str root "/" db)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("init"))
      (some? cmd) (println "Error: init accepts no arguments")
      (.exists (io/file dir)) (println "Error:" db "directory already exists")
      :else (do (io/make-parents (str dir "/objects/foo"))
                (io/make-parents (str dir "/refs/heads/foo"))
                (spit (str dir "/HEAD") "ref: refs/heads/master\n")
                (println "Initialized empty Idiot repository in" db "directory")))))

(defn- file-autocomplete [root db dir file]
  (if (= (count file) 38)
    file
    (->> (str root "/" db "/objects/" dir)
         io/file
         file-seq
         (filter #(.isFile %))
         (map #(.getName %))
         (filter #(= (subs % 0 (count file)) file))
         first)))

(defn- split-path [address {:keys [root db]}]
  (let [dir (subs address 0 2)
        file (subs address 2)
        complete-file (file-autocomplete root db dir file)]
    (str dir "/" complete-file)))

(defn- generate-path [opts address]
  (let [root (:root opts)
        db (:db opts)]
    (str root "/" db "/objects/" (split-path address opts))))

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

(defn- cat-tree-recur [entry opts]
  (when (seq entry)
    (let [mode (util/bytes->str (first (util/split-at-byte 32 entry)))
          mode (if (= "40000" mode) "040000" mode)
          mode-rest-bytes (second (util/split-at-byte 32 entry))
          filename (util/bytes->str (first (util/split-at-byte 0 mode-rest-bytes)))
          addr-bytes-and-rest (second (util/split-at-byte 0 mode-rest-bytes))
          addr-bytes (first (split-at 20 addr-bytes-and-rest))
          rest-entries (last (split-at 20 addr-bytes-and-rest))
          addr (util/to-hex-string addr-bytes)
          obj (db/get-object opts addr)
          type (util/get-object-type obj)
          entry-format (str "%s %s %s\t%s")
          entry-str (format entry-format mode type addr filename)]
      (println entry-str)
      (cat-tree-recur rest-entries opts))))

(defn- cat-tree [opts address]
  (let [bytes (get-object opts address)
        removed-header (second (util/split-at-byte 0 bytes))]
    (cat-tree-recur removed-header opts)))

(defn cat-file [opts args]
  (let [h (or (= (first args) "-h") (= (first args) "--help"))
        p (= (first args) "-p")
        t (= (first args) "-t")
        address (second args)
        root (:root opts)
        db (:db opts)]
    (cond
      h (help '("cat-file"))
      (not (.exists (io/file (str root "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (and (not p) (not t)) (println "Error: the -p or -t switch is required")
      (nil? address) (println "Error: you must specify an address")
      (->> address (generate-path opts) io/file .exists not) (println "Error: that address doesn't exist")
      t (println (util/get-object-type (get-object opts address)))
      (= "tree" (util/get-object-type (get-object opts address))) (cat-tree opts address)
      :else (->> args second (generate-path opts) io/file io/input-stream unzip util/bytes->str remove-header print))))