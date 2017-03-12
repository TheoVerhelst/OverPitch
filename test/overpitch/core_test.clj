(ns overpitch.core-test
  (:require [clojure.test :refer :all]
            [overpitch.core :refer :all]
            [clojure.math.numeric-tower :as math]
  )
)

(def epsilon 0.0000001)

(defn are-close
  "Checks whether the difference between x and y is less than epsilon"
  [x y]
  (< (math/abs (- x y)) epsilon)
)

(deftest add-at-index-test
  (testing "Testing the add-at-index function"
    (is
      (= [0 1 2 3 14 15 16] (add-at-index [0 1 2 3 4 5 6] [10 10 10] 4))
      "Testing regular use case behaviour"
    )
    (is
      (= [0 1 2 3 0 0 0 10 11 12] (add-at-index [0 1 2 3] [10 11 12] 7) )
      "Testing when adding past the end"
    )
    (is
      (= [0 1 12 13 10] (add-at-index [0 1 2 3] [10 10 10] 2))
      "Testing when adding across the end"
    )
  )
)

(deftest hann-window-test
  (testing "Testing common values of hann window function"
    (is (== 0 (hann-window (- 3.1))))
    (is (== 0 (hann-window 0)))
    (is (== 0 (hann-window 1)))
    (is (== 0 (hann-window 2.3)))
    (is (are-close 1 (hann-window 0.5)))
    (is (are-close 0.5 (hann-window 0.25)))
    (is (are-close 0.5 (hann-window 0.75)))
    (is (< 0 (hann-window 0.1) 0.5))
    (is (< 0 (hann-window 0.82) 0.5))
    (is (< 0.5 (hann-window 0.44) 1))
    (is (< 0.5 (hann-window 0.57) 1))
  )
)

(deftest apply-hann-window-test
  (testing "Testing the hann window transformation on a frame"
    (is (every? true? (map are-close [0 0.5 1 0.5 0] (apply-hann-window [1 1 1 1 1]))))
    (is (every? true? (map are-close [0 (- 0.5) -1 (- 0.5) 0] (apply-hann-window [-1 -1 -1 -1 -1]))))
  )
)

(deftest split-channels-test
  (testing "Testing channels splitting"
    (is
      (=
        '([1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16])
        (split-channels [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] 1)
      )
    )
    (is
      (=
        '([1 3 5 7 9 11 13 15] [2 4 6 8 10 12 14 16])
        (split-channels [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16] 2)
      )
    )
    (is
      (=
        '([1 4 7 10 13] [2 5 8 11 14] [3 6 9 12 15])
        (split-channels [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15] 3)
      )
    )
  )
)

(deftest merge-channels-test
  (testing "Testing channels merging"
    (is
      (=
        [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]
        (merge-channels '([1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]))
      )
    )
    (is
      (=
        [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]
        (merge-channels '([1 3 5 7 9 11 13 15] [2 4 6 8 10 12 14 16]))
      )
    )
    (is
      (=
        [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]
        (merge-channels '([1 4 7 10 13] [2 5 8 11 14] [3  6 9 12 15]))
      )
    )
  )
)

(deftest resample-test
  (testing "Testing resampling"
    (is
      (=
        [1 0 -1 0 1 0 -1 0 1 0 -1 -1]
        (resample [1 -1 1 -1 1 -1] 2)
      )
      "Simple interpolation between 1s and -1s"
    )
    (is
      (=
        [-1 -0.75 -0.5 -0.25 0 0.25 0.5 0.625 0.75 0.875 1 0 -1 0 1 0.625 0.25 0.25]
        (resample [-1 -0.5 0 0.5 0.75 1 -1 1 0.25] 2)
      )
      "A little bit more complicated case"
    )
    (is
      (=
        [0 0.3341 0.54 -0.5311 0.989]
        (resample [0 0.44 0.3341 0.111233 0.54 -0.4121 -0.5311 -0.7873 0.989 0.9] 0.5)
      )
    )
  )
)
