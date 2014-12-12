(ns clusters.core-test
  (:require [clojure.test :refer :all]
            [clusters.core :as core]))

(deftest test-distance-fns
  (testing "Distance function"
    (testing "'euclid'"
      (is (= (core/euclid '(12 3 12 6) '(4 23 64 1))
             3193.0)))
    (testing "'hamming'"
      (is (= (core/hamming '(12 3 12 6 5) '(4 3 64 1 5))
             3)))))

(deftest test-calc-accept-condition
  (testing "Calculation accept condition"
    (testing " returns 'true'"
      (is (= (core/calc-accept-condition 23 5 8) true)))
    (testing " returns 'false'"
      (is (= (core/calc-accept-condition 2 5 42) false)))))

(deftest test-set-points-potentials
  (testing "Calsulation of points potentials"
    (is (= (core/set-points-potentials '[{:values [12 3 12 6]} {:values [23 3 1 4]}] core/hamming)
           '({:potential 1.2635971381157267, :values [12 3 12 6]} {:potential 1.2635971381157267, :values [23 3 1 4]})))))
