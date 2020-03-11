(ns tree
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [db :as db]
            [help :as help]
            [util :as util])
  (:import java.io.File))

(def dir "test-dir")

(defrecord FileSystemEntry [type parent-path name contents])

(defn ->FileEntry [parent-path name]
  (let [file (io/file parent-path name)]
    (->FileSystemEntry :file parent-path name (slurp file))))

(declare ->Entry)

(defn ->DirEntry [parent-path name]
  (let [file (io/file parent-path name)
        dir-path (str parent-path File/separator name)
        child->entry #(->Entry dir-path %)
        contents (->> file .list (mapv child->entry))]
    (->FileSystemEntry :dir parent-path name contents)))

(defn ->Entry [parent-path name]
  (let [file (io/file parent-path name)]
    (assert (.exists file))
    (if (.isDirectory file)
      (->DirEntry parent-path name)
      (->FileEntry parent-path name))))

(defn remove-subdir [entry subdir-name]
  (letfn [(filter-by-name [entries]
            (filterv #(not= subdir-name (:name %)) entries))]
    (update entry :contents filter-by-name)))

(declare store-entry)

;; Note: each of the following 3 functions, when completed, is expected to
;; return the address of the thing that it saved.

(defn store-blob-entry [{:keys [contents]} {:keys [root db] :as opts}]
  (let [header+blob (util/add-header "blob" contents)
        blob-addr (util/sha-bytes (.getBytes header+blob))
        hex-str (util/to-hex-string blob-addr)]
    (db/save-to-db header+blob hex-str opts)
    hex-str))

(def modes {:dir 40000 :file 100644})

(defn tree-to-db [tree-contents opts]
  (let [tree-addr (util/sha-bytes (.getBytes tree-contents))
        hex-str (util/to-hex-string tree-addr)]
    (db/save-to-db tree-contents hex-str opts)
    hex-str))

(defn store-tree-entry [{:keys [type parent-path name contents]} {:keys [root db] :as opts}]
  (let [store-entry-with-opts #(store-entry % opts)
        entries+addresses (mapv (juxt identity store-entry-with-opts) contents)
        ;; entry->debug-str (fn [[{:keys [name]} addr]] (str name "@" addr))
        entry->str (fn [[{:keys [name]} addr]] (str (get modes type) " " name \u0000 addr))
        entries-str (as-> entries+addresses $
                          (map entry->str $)
                          (apply str $))
                          ;(str/replace $ #"\n" "\\\\n"))
        dir-debug-str (format "[dir(%s): %s]" name entries-str)]
    (println entries+addresses)
    (println 'store-tree-entry dir-debug-str)
    (println entries-str)
    dir-debug-str))

(defn store-entry [{:keys [type] :as entry} {:keys [root db] :as opts}]
  (if (= type :file)
    (store-blob-entry entry opts)
    (store-tree-entry entry opts)))

(comment
  (pprint (->Entry "." dir))
  (pprint (remove-subdir (->Entry "." dir) ".idiot"))
  (store-entry (remove-subdir (->Entry "." dir) ".idiot")))

(defn- filter-empty-directories [entry]
  (if (= :dir (:type entry))
    (if (empty? (:contents entry))
      nil
      (->
        (update entry :contents #(mapv filter-empty-directories %)) ; recur
        (update :contents #(filter some? %))                ; remove null
        (update :contents #(sort-by :name %))               ; abc order
        (update :contents #(sort-by :type %))))             ; sort so :dir come before :file
    entry))

; can be run from repl but using "(store-root {:root "./test-dir" :db ".idiot"})"

(defn- store-root [{:keys [root db] :as opts}]
  (let [entry
        (->
          (filter-empty-directories (->Entry root ""))
          (remove-subdir db))]
    (store-entry entry opts)))

(defn write-wtree [opts args]
  (let [cmd (first args)
        h (or (= cmd "-h") (= cmd "--help"))
        root (:root opts)
        db (:db opts)]
    (cond
      h (help/help '("write-wtree"))
      (not (.exists (io/file (str root "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (some? cmd) (println "Error: write-wtree accepts no arguments")
      :else (store-root {:root root :db db}))))

(defn commit-tree [opts args]
  (let [address (first args)
        h (or (= address "-h") (= address "--help"))
        m (= (second args) "-m")
        m-value (and (>= (count args) 3) (nth args 2))
        p (and (>= (count args) 4) (= "-p" (nth args 3)))
        p-value (and (>= (count args) 5) (nth args 4))
        root (:root opts)
        db (:db opts)]
    (cond
      h (help/help '("commit-tree"))
      (not (.exists (io/file (str root "/" db)))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (nil? address) (println "Error: you must specify a tree address.")
      (not m) (println "Error: you must specify a message.")
      (not m-value) (println "Error: you must specify a message with the -m switch.")
      (and p (not p-value)) (println "Error: you must specify a commit object with the -p switch."))))