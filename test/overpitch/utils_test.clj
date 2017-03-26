(ns overpitch.utils-test
  (:require [clojure.test :refer :all]
            [overpitch.utils :refer :all]))

(deftest add-at-index-test
  (testing "Testing the add-at-index function"
    (is (= [0 1 2 3 14 15 16] (add-at-index [0 1 2 3 4 5 6] [10 10 10] 4))
        "Testing regular use case behaviour")
    (is (= [0 1 2 3 0 0 0 10 11 12] (add-at-index [0 1 2 3] [10 11 12] 7) )
        "Testing when adding past the end")
    (is (= [0 1 12 13 10] (add-at-index [0 1 2 3] [10 10 10] 2))
        "Testing when adding across the end")))
