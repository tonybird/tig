(ns util
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import java.security.MessageDigest
           (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.util.zip InflaterInputStream DeflaterOutputStream)))

(defn sha-bytes [bytes]
  (.digest (MessageDigest/getInstance "sha1") bytes))

(defn sha-data [data]
  (sha-bytes (.getBytes data)))

(defn to-hex-string
  "Convert the given byte array into a hex string, 2 characters per byte."
  [bytes]
  (letfn [(to-hex [byte]
            (format "%02x" (bit-and 0xff byte)))]
    (->> bytes (map to-hex) (apply str))))

(defn sha1-sum [header+blob]
  (to-hex-string (sha-data header+blob)))

(defn zip
  "Zip the given data with zlib. Return a ByteArrayInputStream of the zipped content."
  [data]
  (let [out (ByteArrayOutputStream.)
        zipper (DeflaterOutputStream. out)]
    (io/copy data zipper)
    (.close zipper)
    (ByteArrayInputStream. (.toByteArray out))))

(defn unzip
  "Unzip the given data with zlib. Pass an input stream as the arg."
  [input-stream]
  (with-open [unzipper (InflaterInputStream. input-stream)
              out (ByteArrayOutputStream.)]
    (io/copy unzipper out)
    (.toByteArray out)))

(defn bytes->str [bytes]
  (->> bytes (map char) (apply str)))

(defn add-header [type object]
  (str type " " (count object) \u0000 object))

(defn add-header-bytes [type object]
  (byte-array (concat (.getBytes (str type " " (count object) \u0000)) object)))

(defn remove-header [header+object]
  (->> (char 0)
       (str/index-of header+object)
       (+ 1)
       (subs header+object)))

(defn split-at-byte [b bytes]
  (let [part1 (take-while (partial not= b) bytes)
        part2 (nthrest bytes (-> part1 count inc))]
    [part1 part2]))

(defn get-object-type [bytes]
  (let [type-bytes (first (split-at-byte 32 bytes))]
    (bytes->str type-bytes)))

(defn dir-to-opts-map [dir]
  (let [db-begins (str/last-index-of dir "/")
        db (subs dir (+ 1 db-begins))
        root (subs dir 0 db-begins)]
    {:root root :db db}))