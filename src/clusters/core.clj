(ns clusters.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file-to-points [file-path]
  (-> (slurp file-path)
      (str/split #"\n")
      (->> (reduce
              (fn [memo line]
                (conj memo (->> (str/split line #",")
                                (butlast)
                                (map #(Double/parseDouble (str/trim %1)))
                                (hash-map :values))))
              []))))

(defn is-file-exist [file-path]
  (.exists (io/file file-path)))

(defn check-and-print [check message]
  (if (not check)
    (println message))
  check)

(defn is-valid-arguments [file-path distance]
  (and
    (check-and-print (is-file-exist file-path) (str "File with path: " file-path " doesn't exist."))
    (check-and-print (or (= distance "euclid") (= distance "hamming")) (str "Invalid distance type: " distance ". Must be either 'euclid' or 'hamming'."))))

(defn -main [file-path distance]
  (if (is-valid-arguments file-path distance)
    (println (read-file-to-points file-path))))