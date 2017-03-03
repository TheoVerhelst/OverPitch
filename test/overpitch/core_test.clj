(ns overpitch.core-test
  (:require [clojure.test :refer :all]
            [overpitch.core :refer :all]
  )
)

(deftest add-at-index-test
  (testing "Testing regular cases behaviour"
    (is
      (= [0 1 2 3 14 15 16]
        (add-at-index [0 1 2 3 4 5 6] [10 10 10] 4)
      )
    )
  )
)
