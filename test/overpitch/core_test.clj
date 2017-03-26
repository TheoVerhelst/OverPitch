(ns overpitch.core-test
  (:require [clojure.test :refer :all]
            [overpitch.core :refer :all]))

(deftest split-channels-test
  (testing "Testing channels splitting"
    (is (= '([1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16])
            (split-channels [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] 1)))
    (is (= '([1 3 5 7 9 11 13 15] [2 4 6 8 10 12 14 16])
            (split-channels [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] 2)))
    (is (= '([1 4 7 10 13] [2 5 8 11 14] [3 6 9 12 15])
            (split-channels [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15] 3)))))

(deftest merge-channels-test
  (testing "Testing channels merging"
    (is (= [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]
           (merge-channels '([1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]))))
    (is (= [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]
           (merge-channels '([1 3 5 7 9 11 13 15] [2 4 6 8 10 12 14 16]))))
    (is (= [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]
           (merge-channels '([1 4 7 10 13] [2 5 8 11 14] [3  6 9 12 15]))))))
