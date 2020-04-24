(ns server
  (:require [switch :as switch]
            [help :as help]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [html5]]))

(defn response [status body]
  {:status status  ; meaning "OK"
   :headers {"Content-Type" "text/html"}  ; instead of e.g. "text/html"
   :body body})  ; the payload

(defn- get-link [type address]
  [:a {:href (str "/" type "/" address)} address])

(defn get-branch-list [dir]
  (let [branch-files (->> (str dir "/refs/heads/") io/file file-seq rest)
        branch-names (sort (map #(.getName %) branch-files))
        name->li #(vector :li (get-link "branch" %))]
    [:ul {:class "branch-list"} (map name->li branch-names)]))

(defn get-head-info [dir]
  (let [head-addr (str dir "/HEAD")
        head-contents (slurp head-addr)
        ref-addr (str/trim-newline (subs head-contents 16))]
    [:div {:class "head-info"} "HEAD points to ref " (get-link "branch" ref-addr)]))

(defn head-response [dir]
  (let [branch-list (get-branch-list dir)
        head-info (get-head-info dir)
        body (html5
               [:head [:title "Explore"]]
               [:body head-info branch-list])]
    (response 200 body)))

(defn branch-response [dir address]
  (let [body (html5
               [:head [:title "Explore"]]
               [:body [:div [:h1 {:class "info"} "Branch"] address]])]
    (response 200 body)))

(defn commit-response [dir address]
  (let [body (html5
               [:head [:title "Explore"]]
               [:body [:div [:h1 {:class "info"} "Commit"] address]])]
    (response 200 body)))

(defn tree-response [dir address]
  (let [body (html5
               [:head [:title "Explore"]]
               [:body [:div [:h1 {:class "info"} "Tree"] address]])]
    (response 200 body)))

(defn blob-response [dir address]
  (let [body (html5
               [:head [:title "Explore"]]
               [:body [:div [:h1 {:class "info"} "Blob"] address]])]
    (response 200 body)))

(defn- parse-uri [uri]
  (-> uri (str/split #"/") (nth 2)))

(defn get-handler [dir]
  (fn handler [request]
    (let [uri (:uri request)
          head? (= uri "/")
          address (and (not head?) (parse-uri uri))]
      (cond
        head? (head-response dir)
        (str/starts-with? uri "/branch/") (branch-response dir address)
        (str/starts-with? uri "/commit/") (commit-response dir address)
        (str/starts-with? uri "/tree/") (tree-response dir address)
        (str/starts-with? uri "/blob/") (blob-response dir address)))))

(defn start-server [dir]
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