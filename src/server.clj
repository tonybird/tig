(ns server
  (:require [switch :as switch]
            [help :as help]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.java.io :as io]
            [hiccup.core :refer [html]]))

(defn get-html [branches] (html [:html
                         [:head
                          [:title "Explore"]]
                         [:body [:ul (map #(vector :li %) branches)]]]))

(defn get-handler [html]
  (fn handler [request]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body html}))

(defn start-server [handler]
  (run-jetty handler {:port 3000}))


(defn explore [{:keys [root db] :as opts} args]
  (let [cmd (first args)
        dir (str root "/" db)]
    (cond
      (or (= cmd "-h") (= cmd "--help")) (help/help '("explore"))
      (= (count args) 1) (println "Error: you must specify a numeric port with -p.")
      :else (let [{n :n ref :ref} (switch/parse-num-non-negative args "-p")
                  files (->> (str dir "/refs/heads/") io/file file-seq rest)
                  filenames (sort (map #(.getName %) files))]
              (println (str "Starting server on port " n "."))
              (start-server (get-handler (get-html filenames)))))))

