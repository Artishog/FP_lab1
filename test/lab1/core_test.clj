(ns lab1.core-test
  (:require [clojure.test :refer :all]
            [lab1.core :refer :all])
  (import java.lang.Math))


(deftest parse-line
  (is (= [1.0 2.0 3.0 4.0] (parse-line "1,2,3,4,5"))))

(deftest parse-file-test
  (is (=[[0.0 3.0] [1.0 5.0] [2.0 4.0]] (parse-file "resources/test.txt"))))

(deftest distance-between-points-test
  (are [x1 x2] (= 36.0 (distance-between-points x1 x2))
       9 3
       3 9
       6 0
       0 6
       1 -5))

(deftest e-distance-test
  (are [x1 x2] (= 5.0 (e-distance x1 x2))
       [3 0] [0 4]
       [2 -2] [-2 1]))

(run-all-tests)
