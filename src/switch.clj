(ns switch
  (:require [clojure.java.io :as io]
            [help :refer [help]]
            [tree :refer [commit-tree]]
            [clojure.string :as str]))

(defn- get-head-pointer [dir]
  (let [head-addr (str dir "/HEAD")
        head-contents (slurp head-addr)
        is-ref? (str/starts-with? head-contents "ref: ")]
    (if is-ref?
      {:path (str/trim-newline (str dir "/" (subs head-contents 5))) :is-ref is-ref?}
      {:path head-addr :is-ref is-ref?})))

(defn- get-ref-address [dir name]
  (if (or (= name "HEAD") (= name "@"))
    (:path (get-head-pointer dir))
    (str dir "/refs/heads/" name)))

(defn rev-parse [opts args]
  (let [arg (first args)
        root (:root opts)
        db (:db opts)
        dir (str root "/" db)]
    (cond
      (or (= arg "-h") (= arg "--help")) (help '("rev-parse"))
      (empty? args) (println "Error: you must specify a branch name.")
      (> (count args) 1) (println "Error: you must specify a branch name and nothing else.")
      (not (.exists (io/file dir))) (println "Error: could not find database. (Did you run `idiot init`?)")
      :else (let [address (get-ref-address dir arg)]
              (if (.exists (io/file address))
                (print (slurp address))
                (println (str "Error: could not find ref named " arg ".")))))))

(defn switch [opts args]
  (let [cmd (first args)
        c (= cmd "-c")
        root (:root opts)
        db (:db opts)
        dir (str root "/" db)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("switch"))
      (empty? args) (println "Error: you must specify a branch name.")
      (or (> (count args) 2) (and (> (count args) 1) (not c))) (println "Error: you may only specify one branch name.")
      (not (.exists (io/file dir))) (println "Error: could not find database. (Did you run `idiot init`?)")
      :else (let [branch-name (if c (second args) (first args))
                  branch-addr (str dir "/refs/heads/" branch-name)
                  exists? (.exists (io/file branch-addr))
                  head-addr (str dir "/HEAD")]
              (cond
                (and c exists?) (println "Error: a ref with that name already exists.")
                (and (not c) (not exists?)) (println "Error: no ref with that name exists.")
                (not c) (do (spit head-addr (str "ref: refs/heads/" branch-name "\n"))
                            (println (str "Switched to branch '" branch-name "'")))
                c (do (spit branch-addr (slurp (get-head-pointer dir)))
                      (spit head-addr (str "ref: refs/heads/" branch-name "\n"))
                      (println (str "Switched to a new branch '" branch-name "'"))))))))

(defn- add-branch-prefix [dir branch-name]
  (let [head-addr (str dir "/HEAD")
        head-contents (slurp head-addr)
        head-branch-name (str/trim-newline (subs head-contents 16))
        match (= branch-name head-branch-name)]
    (if match
      (str "* " branch-name)
      (str "  " branch-name))))

(defn- print-branches [dir]
  (let [files (->> (str dir "/refs/heads/") io/file file-seq rest)
        filenames (sort (map #(.getName %) files))
        names-with-prefixes (map #(add-branch-prefix dir %) filenames)]
    (doseq [str names-with-prefixes]
      (println str))))

(defn- delete-branch [dir branch-name]
  (let [path (str dir "/refs/heads/" branch-name)
        head-contents (slurp (str dir "/HEAD"))
        head-branch-name (str/trim-newline (subs head-contents 16))]
    (cond
      (-> path io/file .exists not) (println (str "Error: branch '" branch-name "' not found."))
      (= head-branch-name branch-name) (println (str "Error: cannot delete checked-out branch '" branch-name "'."))
      :else (do (io/delete-file path)
                (println (str "Deleted branch " branch-name "."))))))

(defn branch [opts args]
  (let [cmd (first args)
        d (= cmd "-d")
        dir (str (:root opts) "/" (:db opts))]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("branch"))
      (and d (= 1 (count args))) (println "Error: you must specify a branch name.")
      (and d (not= (count args) 2)) (println "Error: invalid arguments.")
      (and (not d) (not= (count args) 0)) (println "Error: invalid arguments.")
      (not (.exists (io/file dir))) (println "Error: could not find database. (Did you run `idiot init`?)")
      d (delete-branch dir (second args))
      :else (print-branches dir))))

(defn commit [{:keys [root db] :as opts} args]
  (let [cmd (first args)
        opts (assoc opts :silent true)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("commit"))
      :else (let [sha (commit-tree opts args)]
              (when sha (do (println "Commit created.")
                            (let [{:keys [path is-ref]} (get-head-pointer (str root "/" db))]
                              (when is-ref (println (str "Updated branch " (str/trim (last (str/split path #"/"))) ".")))
                              (spit path sha))))))))