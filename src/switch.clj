(ns switch
  (:require [clojure.java.io :as io]
            [help :refer [help]]
            [db :as db]
            [util :as util]
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
                c (do (spit branch-addr (slurp (:path (get-head-pointer dir))))
                      (spit head-addr (str "ref: refs/heads/" branch-name "\n"))
                      (println (str "Switched to a new branch '" branch-name "'"))))))))

(defn- add-branch-prefix [dir branch-name]
  (let [head-addr (str dir "/HEAD")
        head-contents (slurp head-addr)
        head-branch-name (str/trim-newline (last (str/split head-contents #"/")))
        match (= branch-name head-branch-name)]
    (if match
      (str "* " branch-name)
      (str "  " branch-name))))

(defn- print-branches [dir]
  (let [files (->> (str dir "/refs/heads/") io/file file-seq rest)
        filenames (sort (map #(.getName %) files))
        names-with-prefixes (map #(add-branch-prefix dir %) filenames)]
    (println (str/join "\n" names-with-prefixes))))

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
              (when sha
                (println "Commit created.")
                (let [{:keys [path is-ref]} (get-head-pointer (str root "/" db))]
                  (when is-ref (println (str "Updated branch " (str/trim (last (str/split path #"/"))) ".")) (spit path (str sha "\n")))))))))

(defn- print-commit [opts commit-addr n]
  (when (not= n 0)
    (let [commit-body (-> (db/get-object opts commit-addr)
                          (util/bytes->str)
                          (str/split #"\n"))
          parent (first (filter #(str/starts-with? % "parent") commit-body))]
      (println commit-addr)
      (when parent (let [parent-addr (subs parent 7)]
                     (print-commit opts parent-addr (- n 1)))))))

(defn- print-commit-oneline [opts commit-addr n]
  (when (not= n 0)
    (let [commit-body (-> (db/get-object opts commit-addr)
                          (util/bytes->str)
                          (str/split #"\n"))
          parent (first (filter #(str/starts-with? % "parent") commit-body))
          title (-> (db/get-object opts commit-addr)
                    (util/bytes->str)
                    (str/split #"\n\n")
                    (last)
                    (str/split #"\n")
                    (first))]
      (println (str (subs commit-addr 0 7) " " title))
      (when parent (let [parent-addr (subs parent 7)]
                     (print-commit-oneline opts parent-addr (- n 1)))))))

(defn parse-num-non-negative [args arg-name]
  (if (= (first args) arg-name)
    {:present true :n (try (Integer/parseInt (nth args 1)) (catch Exception _ (println (str "Error: the argument for '" arg-name "' must be a non-negative integer.")) :fail)) :ref (last args)}
    {:present false :n 3000 :ref (first args)}))

(defn- rev-list-get-file-name [dir ref n _]
  (if (number? n)
    (get-ref-address dir "@")
    (get-ref-address dir ref)))

(defn rev-list [{:keys [root db] :as opts} args]
  (let [cmd (first args)
        dir (str root "/" db)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("rev-list"))
      (and (= cmd "-n") (= (count args) 1)) (println "Error: you must specify a numeric count with '-n'.")
      (not (.exists (io/file dir))) (println "Error: could not find database. (Did you run `idiot init`?)")
      :else (let [{n :n ref :ref pres :present} (parse-num-non-negative args "-n")
                  file-name (rev-list-get-file-name dir ref n pres)]
              (cond
                (= n :fail) nil
                (not (.exists (io/file file-name))) (println (str "Error: could not find ref named " cmd "."))
                :else (print-commit opts (.trim (slurp file-name)) n))))))

(defn log [{:keys [root db] :as opts} args]
  (let [cmd (first args)
        dir (str root "/" db)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help '("log"))
      (not (.exists (io/file dir))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (not= cmd "--oneline") (println "Error: log requires the --oneline switch")
      (and (> (count args) 1) (= (nth args 1) "-n") (= (count args) 2)) (println "Error: you must specify a numeric count with '-n'.")
      :else (let [{n :n ref :ref pres :present} (parse-num-non-negative (rest args) "-n")
                  file-name (rev-list-get-file-name dir ref n pres)]
              (cond
                (= n :fail) nil
                (not (.exists (io/file file-name))) (println (str "Error: could not find ref named " ref "."))
                :else (let [commit (.trim (slurp file-name))] (print-commit-oneline opts commit n)))))))
