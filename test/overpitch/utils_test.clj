(ns overpitch.utils-test
  (:require [clojure.test :refer :all]
            [overpitch.utils :refer :all]))

(deftest add-at-index-test
  (testing "Testing the add-at-index function"
    (is (= [0 1 2 3 14 15 16] (add-at-index [0 1 2 3 4 5 6] [10 10 10] 4))
        "Testing regular use case behaviour")
    (is (= [0 1 2 3 0 0 0 10 11 12] (add-at-index [0 1 2 3] [10 11 12] 7))
        "Testing when adding past the end")
    (is (= [0 1 12 13 10] (add-at-index [0 1 2 3] [10 10 10] 2))
        "Testing when adding across the end")))

(deftest merge-channels-test
  (testing "Testing channels merging"
    (is (= [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]
           (merge-channels '([1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]))))
    (is (= [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]
           (merge-channels '([1 3 5 7 9 11 13 15] [2 4 6 8 10 12 14 16]))))
    (is (= [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]
           (merge-channels '([1 4 7 10 13] [2 5 8 11 14] [3  6 9 12 15]))))))

(deftest split-channels-test
  (testing "Testing channels splitting"
    (is (= '([1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16])
            (split-channels [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] 1)))
    (is (= '([1 3 5 7 9 11 13 15] [2 4 6 8 10 12 14 16])
            (split-channels [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] 2)))
    (is (= '([1 4 7 10 13] [2 5 8 11 14] [3 6 9 12 15])
            (split-channels [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15] 3)))))
