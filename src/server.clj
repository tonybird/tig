(ns server
  (:require [switch :as switch]
            [help :as help]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [html5]]))

(defn- response [body-element]
  {:status 200  ;; OK
   :headers {"Content-Type" "text/html"}
   :body (html5 [:head [:title "Explore"]] body-element)})

(defn- response-300 [body-element]
  {:status 300  ;; Multiple Choices
   :headers {"Content-Type" "text/html"}
   :body (html5 [:head [:title "Explore"]] body-element)})

(defn- response-302 [location]
  {:status 302  ;; Found
   :headers {"Content-Type" "text/html"
             "Location" location}})

(defn- response-404 []
  {:status 404  ;; Not Found
   :headers {"Content-Type" "text/html"}})

(defn- get-link [type address]
  [:a {:href (str "/" type "/" address)} address])

;; 1. The / endpoint: HEAD info and branch list ;;

(defn- branch-list-element [dir]
  (let [branch-files (->> (str dir "/refs/heads/") io/file file-seq rest)
        branch-names (sort (map #(.getName %) branch-files))
        name->li #(vector :li (get-link "branch" %))]
    [:ul {:class "branch-list"} (map name->li branch-names)]))

(defn- head-info-element [dir]
  (let [head-addr (str dir "/HEAD")
        head-contents (slurp head-addr)
        ref-addr (str/trim-newline (subs head-contents 16))]
    [:div {:class "head-info"} "HEAD points to ref " (get-link "branch" ref-addr)]))

(defn- head-response [dir]
  (let [branch-list (branch-list-element dir)
        head-info (head-info-element dir)
        body-element [:body head-info branch-list]]
    (response body-element)))

;; 2. /branch/<branch>: log --oneline output with linked commit IDs ;;

(defn- get-commit-list [dir commit-addr prev-commits]
  (let [opts (util/dir-to-opts-map dir)
        commit-body (-> (db/get-object opts commit-addr)
                          (util/bytes->str)
                          (str/split #"\n"))
        parent (first (filter #(str/starts-with? % "parent") commit-body))
        message (-> (db/get-object opts commit-addr)
                    (util/bytes->str)
                    (str/split #"\n\n")
                    (last)
                    (str/split #"\n")
                    (first))
        prev-commits (conj prev-commits {:commit-addr (subs commit-addr 0 7),
                                         :message message})]
    (if parent (let [parent-addr (subs parent 7)]
                     (get-commit-list dir parent-addr prev-commits))
               prev-commits)))

(defn- commit-list-li [addr message]
  (vector :li (get-link "commit" addr) (str " " message)))

(defn- commit-list-ul [dir branch]
  (let [ref-filename (str dir "/refs/heads/" branch)
        commit-addr (.trim (slurp ref-filename))
        commits (get-commit-list dir commit-addr '())
        commit->li #(commit-list-li (:commit-addr %) (:message %))]
    [:ul {:class "commit-list"} (map commit->li commits)]))

(defn- branch-response [dir branch]
    (let [ref-address (switch/get-ref-address dir branch)
          ref-exists? (.exists (io/file ref-address))]
      (if ref-exists?
        (response [:body (commit-list-ul dir branch)])
        (response-404))))

;; 3. /commit/<address>: show commit, with linked parents and linked tree ;;

(defn- disambiguation-li [dir full-addr]
  (let [opts (util/dir-to-opts-map dir)
        type (util/get-object-type (db/get-object opts full-addr))]
  [:li (get-link type full-addr) (str " " type)]))

(defn- disambiguation-response [dir full-addr-list]
  (let [list-items (map #(disambiguation-li dir %) full-addr-list)
        body [:body
              [:p "The given address prefix is ambiguous. Please disambiguate
                  your intent by choosing from the following options."]
              [:ul {:class "disambiguation-list"} list-items]]]
    (response-300 body)))

(defn- fix-gt-lt [s]
  (-> s (str/replace #"<" "&lt;") (str/replace #">" "&gt;")))

(defn- format-parent [line]
  (let [addr (subs line 7)]
    [:div {:class "parent"} "parent " (get-link "commit" addr)]))

(defn- format-tree [line]
  (let [addr (subs line 5)]
    [:div {:class "tree"} "tree " (get-link "tree" addr)]))

(defn- commit-response [dir addr]
  (let [opts (util/dir-to-opts-map dir)
        contents-str (->> addr
                      (db/generate-path opts)
                      io/file io/input-stream
                      util/unzip util/bytes->str util/remove-header
                      fix-gt-lt)
        contents (str/split-lines contents-str)
        tree (->> contents (filter #(str/starts-with? % "tree")) first format-tree)
        parents (->> contents (filter #(str/starts-with? % "parent")) (map format-parent))
        author (->> contents (filter #(str/starts-with? % "author")) first)
        committer (->> contents (filter #(str/starts-with? % "committer")) first)
        message (as-> contents c
                      (.indexOf c "")
                      (subvec contents c)
                      (str/join "\n" c))
        body [:body
              [:h1 "Commit " addr]
              tree
              parents
              [:div {:class "author"} author]
              [:div {:class "committer"} committer]
              [:pre {:class "message"} message]]]
    (response body)))

;; 4. /tree/<address>: show tree, with linked trees and linked blobs ;;

(defn- tree-recur [entry opts l]
    (if (seq entry)
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
            link (get-link type addr)
            e [:li [:tt (str mode " " type " ") link (str " " filename)]]]
        (do
          (tree-recur rest-entries opts (conj l e))))
      l))

(defn- tree-response [dir addr]
  (let [opts (util/dir-to-opts-map dir)
        bytes (db/get-object opts addr)
        removed-header (second (util/split-at-byte 0 bytes))
        entry-list (tree-recur removed-header opts '())
        body [:body
              [:div [:h1 "Tree " addr]]
              [:ul {:class "tree-entries"} entry-list]]]
    (response body)))

;; 5. /blob/<address>: show blob data ;;

(defn- blob-response [dir address]
  (let [body [:body [:div [:h1 "Blob " address]]]]
    (response body)))

(defn- address-response [dir addr expected-type]
  (let [opts (util/dir-to-opts-map dir)
        full-addr-list (db/file-autocomplete (:root opts) (:db opts) addr)
        no-match? (empty? full-addr-list)
        ambiguous? (> (count full-addr-list) 1)]
    (cond
      no-match? (response-404)
      ambiguous? (disambiguation-response dir full-addr-list)
      :else (let [addr (first full-addr-list)
                  not-found? (->> addr (db/generate-path opts) io/file .exists not)
                  type (and (not not-found?) (util/get-object-type (db/get-object opts addr)))]
              (cond not-found? (response-404)
                    (not= type expected-type) (response-302 (str "/" type "/" addr))
                    (= type "commit") (commit-response dir addr)
                    (= type "tree") (tree-response dir addr)
                    (= type "blob") (blob-response dir addr))))))

;; HTTP stuff

(defn- get-handler [dir]
  (fn handler [request]
    (let [uri (:uri request)
          head? (= uri "/")
          uri->addr #(-> % (str/split #"/") (nth 2))]
      (cond
        head? (head-response dir)
        (str/starts-with? uri "/branch/") (branch-response dir (uri->addr uri))
        (str/starts-with? uri "/commit/") (address-response dir (uri->addr uri) "commit")
        (str/starts-with? uri "/tree/") (address-response dir (uri->addr uri) "tree")
        (str/starts-with? uri "/blob/") (address-response dir (uri->addr uri) "blob")
        :else (response-404)))))

(defn- start-server [dir]
  (let [handler (get-handler dir)]
  (run-jetty handler {:port 3000})))

(defn explore [{:keys [root db] :as _} args]
  (let [cmd (first args)
        dir (str root "/" db)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help/help '("explore"))
      (not (.exists (io/file dir))) (println "Error: could not find database. (Did you run `idiot init`?)")
      (= (count args) 1) (println "Error: you must specify a numeric port with '-p'.")
      :else (let [{n :n _ :ref} (switch/parse-num-non-negative args "-p")]
              (cond
                (= n :fail) nil
                :else ((println (str "Starting server on port " n ".")) (start-server dir)))))))