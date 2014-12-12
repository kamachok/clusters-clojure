(ns clusters.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def r-a 3)
(def alpha-param (/ 4 (Math/pow r-a 2)))
(def r-b (* 1.5 r-a))
(def betta-param (/ 4 (Math/pow r-b 2)))
(def e_top 0.5)
(def e_bottom 0.15)

(defn exponent [param value]
  (Math/exp (- (* param value))))

(defn set-points-potentials
  [points distance-fn]
  (->> (map (fn [point]
              (assoc
                point
                :potential
                (reduce
                  (fn [memo other]
                    (+ memo
                       (exponent
                         alpha-param
                         (distance-fn
                           (:values point)
                           (:values other))))) 0 points))) points)
       (sort-by :potential >)))

(defn revise-points-potentials
  [points distance-fn center]
  (->> (map (fn [point]
              (->> (distance-fn (:values point) (:values center))
                   (exponent betta-param)
                   (* (:potential center))
                   (- (:potential point))
                   (assoc point :potential))) points)
       (sort-by :potential >)))

(defn calc-max-potential
  [points]
  (->> points
       (sort-by :potential >)
       (first)))

(defn calc-min-distance [centers distance-fn next-center]
  (->> (map #(distance-fn (:values next-center) (:values %1)) centers)
       (apply min)))

(defn calc-accept-condition [min-distance next first]
  (<= 1 (+ (/ min-distance r-a) (/ next first))))

(defn classify [potentials distance-fn first-center centers index]
  (let [revised-potentials (if (= index 0) potentials (revise-points-potentials potentials distance-fn ((comp first reverse) centers)))
        next-center (if (= index 0) (second revised-potentials) (first revised-potentials))
        first-potential (:potential first-center)
        next-potential (:potential next-center)
        max-condition (* first-potential e_top)
        min-condition (* first-potential e_bottom)]
    (cond
      (> next-potential max-condition) (recur revised-potentials
                                              distance-fn
                                              first-center
                                              (conj centers next-center)
                                              (inc index))
      (< next-potential min-condition) centers
      :else (let [min-distance (calc-min-distance centers distance-fn next-center)
                  accept-condition (calc-accept-condition min-distance next-potential first-potential)]
              (if accept-condition (recur revised-potentials
                                          distance-fn
                                          first-center
                                          (conj centers next-center)
                                          (inc index))
                                   (recur (conj (rest revised-potentials) (assoc next-center :potential 0))
                                          distance-fn
                                          first-center
                                          (conj centers (calc-max-potential (rest revised-potentials)))
                                          (inc index)))))))

(defn find-cluster-centers [points distance-fn]
  (let [start-potentials (set-points-potentials points distance-fn)
        first-center (first start-potentials)]
    (classify start-potentials distance-fn first-center [first-center] 0)))

(defn euclid [values1 values2]
  (->> (map #(Math/pow (- %1 %2) 2) values1 values2)
       (reduce +)))

(defn hamming [values1 values2]
  (->> (map #(not= %1 %2) values1 values2)
       (remove false?)
       (count)))

(defn distance-fn [distance]
  (case distance
    "euclid" euclid
    "hamming" hamming))

(defn read-file-to-points [file-path]
  (-> (slurp file-path)
      (str/split #"\n")
      (->> (reduce
             (fn [memo line]
               (conj memo (->> (str/split line #",")
                               (butlast)
                               (map #(Double/parseDouble (str/trim %1)))
                               (hash-map :values)))) []))))

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
    (let [distance-fn (distance-fn distance)
          points (read-file-to-points file-path)
          centers (find-cluster-centers points distance-fn)]
      (println "Finded: " (count centers))
      (dorun (map #(println "Potential" (:potential %1) ", values" (:values %1)) centers)))))
