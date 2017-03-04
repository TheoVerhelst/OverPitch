(ns overpitch.core-test
  (:require [clojure.test :refer :all]
            [overpitch.core :refer :all]
  )
)

(deftest add-at-index-test
  (testing "Testing regular use case behaviour"
    (is
      (= [0 1 2 3 14 15 16]
        (add-at-index [0 1 2 3 4 5 6] [10 10 10] 4)
      )
    )
  )
  (testing "Testing when adding past the end"
    (is
      (= [0 1 2 3 0 0 0 10 11 12]
        (add-at-index [0 1 2 3] [10 11 12] 7)
      )
    )
  )
  (testing "Testing when adding across the end"
    (is
      (= [0 1 12 13 10]
        (add-at-index [0 1 2 3] [10 10 10] 2)
      )
    )
  )
)

(deftest hann-window-test
  (testing "Testing common values of hann window function"
    (is (== 0 (hann-window (- 3.1))))
    (is (== 0 (hann-window 0)))
    (is (== 0 (hann-window 1)))
    (is (== 0 (hann-window 2.3)))
    (is (== 1 (hann-window 0.5)))
    (is (< 0 (hann-window 0.1) 0.5))
    (is (< 0 (hann-window 0.82) 0.5))
    (is (< 0.5 (hann-window 0.44) 1))
    (is (< 0.5 (hann-window 0.57) 1))
  )
)
