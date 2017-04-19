(ns overpitch.resampling-test
  (:require [clojure.test :refer :all]
            [overpitch.resampling :refer :all]
            [overpitch.utils :refer :all]))

(deftest linear-interpolation-test
  (testing "Testing linear interpolation"
    (is (== (linear-interpolation [0 0.3 -0.6 1] 0) 0.3))
    (is (== (linear-interpolation [0 0.12 -0.33 1] 1) -0.33))
    (is (== (linear-interpolation [0 0.2 0.8 1] 0.5) 0.5))
    (is (== (linear-interpolation [0 0.2 -0.3 1] 0.8) -0.2))))

(deftest cubic-spline-interpolation-test
  (testing "Testing cubic spline interpolation"
    (is (== (cubic-spline-interpolation [0.2 0.2 0.2 0.2] 0.3) 0.2) "with constant values")
    (is (== (cubic-spline-interpolation [0 1 0.25 1] 1) 0.25))
    (is (== (cubic-spline-interpolation [1 1 -1 1] 1) -1))
    (is (== (cubic-spline-interpolation [0 -0.8 0.3 1] 0) -0.8))
    (is (almost-equal (cubic-spline-interpolation [0.5 -1 -0.25 1] 0.75) -0.52539062))))

(deftest resample-test
  (testing "Testing resampling"
    ; There are 0.125s because the interpolation takes v[-1] = v[0] and
    ; v[n] = v[n - 1] for past-the-end and before-the-beginning values.
    ; According to cubic splines, the last value should be -1.125, but
    ; we expect the resampling to clip the output.
    (is (almost-equal [1 -0.125 -1 0 1 0 -1 0 1 0.125 -1 -1]
           (resample [1 -1 1 -1 1 -1] 2))
        "Simple interpolation between 1s and -1s")

    (is (= [0 0.3341 0.54 -0.5311 0.989]
           (resample [0 0.44 0.3341 0.111233 0.54 -0.4121 -0.5311 -0.7873 0.989 0.9] 0.5))
        "Should discard half of the input samples")))
