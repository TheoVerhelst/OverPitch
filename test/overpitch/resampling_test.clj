(ns overpitch.resampling-test
  (:require [clojure.test :refer :all]
            [overpitch.resampling :refer :all]))

(deftest resample-test
  (testing "Testing resampling"
    (is (= [1 0 -1 0 1 0 -1 0 1 0 -1 -1]
           (resample [1 -1 1 -1 1 -1] 2))
        "Simple interpolation between 1s and -1s")

    (is (= [-1 -0.75 -0.5 -0.25 0 0.25 0.5 0.625 0.75 0.875 1 0 -1 0 1 0.625 0.25 0.25]
           (resample [-1 -0.5 0 0.5 0.75 1 -1 1 0.25] 2))
        "A little bit more complicated case")

    (is (= [0 0.3341 0.54 -0.5311 0.989]
           (resample [0 0.44 0.3341 0.111233 0.54 -0.4121 -0.5311 -0.7873 0.989 0.9] 0.5))
        "Should discard half of the input samples")))
