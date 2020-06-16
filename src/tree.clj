(ns tree
  (:require [clojure.java.io :as io]
            [db :as db]
            [help :as help]
            [util :as util]
            [clojure.string :as str])
  (:import java.io.File))

(def modes {:dir 40000 :file 100644})

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

(defn store-blob-entry [{:keys [contents]} opts]
  (let [header+blob (util/add-header "blob" contents)
        blob-addr (util/sha-bytes (.getBytes header+blob))
        hex-str (util/to-hex-string blob-addr)
        address-exists (db/address-exists? opts hex-str)]
    (if (not address-exists) (db/save-to-db header+blob hex-str opts) nil)
    blob-addr))

(defn tree-to-db [tree-contents opts]
  (let [tree-addr (util/sha-bytes (.getBytes tree-contents))
        hex-str (util/to-hex-string tree-addr)]
    (db/save-to-db tree-contents hex-str opts)
    hex-str))

;tree [length in bytes]\000[list of entries]

(defn- get-entry-byte-array [{:keys [type hash name]}]
  (byte-array (concat (.getBytes (str (type modes) " " name \u0000)) hash)))

(defn store-tree-entry [{:keys [_type _parent-path _name contents]} opts]
  (let [stored (->>
                (map #(store-entry % opts) contents)
                (map #(get-entry-byte-array %))
                (apply concat))
        header+blob (util/add-header-bytes "tree" stored)
        tree-addr (util/sha-bytes header+blob)
        hex-str (util/to-hex-string tree-addr)]
    (db/save-to-db header+blob hex-str opts)
    tree-addr))

(defn store-entry [{:keys [type] :as entry} opts]
  (if (= type :file)
    {:type :file :hash (store-blob-entry entry opts) :name (:name entry)}
    {:type :dir :hash (store-tree-entry entry opts) :name (:name entry)}))

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

; can be run from repl but using "(store-root {:root "./test-dir" :db ".tig"})"

(defn- store-root [{:keys [root db] :as opts}]
  (let [entry
        (->
         (filter-empty-directories (->Entry root ""))
         (remove-subdir db))]
    (if (empty? (:contents entry))
      (do (println "The directory was empty, so nothing was saved.") nil)
      (store-entry entry opts))))

(defn write-wtree [opts args]
  (let [cmd (first args)
        h (or (= cmd "-h") (= cmd "--help"))
        root (:root opts)
        db (:db opts)]
    (cond
      h (help/help '("write-wtree"))
      (not (.exists (io/file (str root "/" db)))) (println "Error: could not find database. (Did you run `tig init`?)")
      (some? cmd) (println "Error: write-wtree accepts no arguments")
      :else (let [hash (:hash (store-root {:root root :db db}))]
              (if (not= nil hash)
                (println (util/to-hex-string hash))
                nil)))))

(defn commit-tree [opts args]
  (let [address (first args)
        h (or (= address "-h") (= address "--help"))
        m (= (second args) "-m")
        message (and (>= (count args) 3) (nth args 2))
        p (if (> (count args) 3) (subvec (vec args) 3) ())
        p-values-raw (filter #(not= "-p" %) p)
        p-values-list (map #(db/file-autocomplete (:root opts) (:db opts) %) p-values-raw)
        p-values (map first p-values-list)
        empty-p-flag (= "-p" (last p))
        no-obj-at-p-addr (some #(when (not (db/address-exists? opts %)) %) p-values)
        not-commit #(not= "commit" (util/get-object-type (db/get-object opts %)))
        non-commit-at-p-addr (and (not no-obj-at-p-addr) (some #(when (not-commit %) %) p-values))
        root (:root opts)
        db (:db opts)
        full-address-list (db/file-autocomplete root db address)
        full-address (first full-address-list)
        verbose? (not (contains? opts :silent))]
    (cond
      h (help/help '("commit-tree"))
      (not (.exists (io/file (str root "/" db)))) (println "Error: could not find database. (Did you run `tig init`?)")
      (nil? address) (println "Error: you must specify a tree address.")
      ; begin check for file-autocomplete
      (< (count address) 4) (println (str "Error: too few characters specified for address '" address "'"))
      (some #(< (count %) 4) p-values-raw) (let [bad-address (-> (filter #(< (count %) 4) p-values-raw) (first))]
                                             (println (str "Error: too few characters specified for address '" bad-address "'")))
      (some #(> (count %) 1) p-values-list) (let [bad-address (-> (filter #(> (count %) 1) p-values-list) (first) (first))
                                                  address (first (filter #(str/starts-with? bad-address %) p-values-raw))]
                                              (println (str "Error: ambiguous match for address '" address "'")))
      (nil? full-address) (println "Error: that address doesn't exist")
      (> (count full-address-list) 1) (println (str "Error: ambiguous match for address '" address "'"))
      ; end
      (not (db/address-exists? opts full-address)) (println "Error: no tree object exists at that address.")
      (not= "tree" (util/get-object-type (db/get-object opts full-address))) (println "Error: an object exists at that address, but it isn't a tree.")
      (not m) (println "Error: you must specify a message.")
      (not message) (println "Error: you must specify a message with the -m switch.")
      no-obj-at-p-addr (println (str "Error: no commit object exists at address " no-obj-at-p-addr "."))
      non-commit-at-p-addr (println (str "Error: an object exists at address " non-commit-at-p-addr ", but it isn't a commit."))
      empty-p-flag (println "Error: you must specify a commit object with the -p switch.")

      ;; Otherwise, write a new commit object and print its address
      :else (let [author-str "Linus Torvalds <torvalds@transmeta.com> 1581997446 -0500"
                  parent-str (reduce #(str %1 "parent " %2 "\n") "" p-values)
                  commit-format (str "tree %s\n"
                                     "%s"
                                     "author %s\n"
                                     "committer %s\n"
                                     "\n"
                                     "%s\n")
                  commit-str (format commit-format
                                     full-address
                                     parent-str
                                     author-str
                                     author-str
                                     message)
                  header+commit (util/add-header "commit" commit-str)
                  commit-addr (util/sha1-sum header+commit)]
              (db/save-to-db header+commit commit-addr opts)
              (when verbose? (println commit-addr))
              commit-addr))))