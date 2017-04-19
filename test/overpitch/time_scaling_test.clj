(ns overpitch.time-scaling-test
  (:require [clojure.test :refer :all]
            [overpitch.time-scaling :refer :all]
            [overpitch.utils :refer :all]))

(deftest hann-window-test
  (testing "Testing common values of hann window function"
    (is (== 0 (hann-window (- 3.1))))
    (is (== 0 (hann-window 0)))
    (is (== 0 (hann-window 1)))
    (is (== 0 (hann-window 2.3)))
    (is (almost-equal 1 (hann-window 0.5)))
    (is (almost-equal 0.5 (hann-window 0.25)))
    (is (almost-equal 0.5 (hann-window 0.75)))
    (is (< 0 (hann-window 0.1) 0.5))
    (is (< 0 (hann-window 0.82) 0.5))
    (is (< 0.5 (hann-window 0.44) 1))
    (is (< 0.5 (hann-window 0.57) 1))))

(deftest apply-hann-window-test
  (testing "Testing the hann window transformation on a frame"
    (is (almost-equal [0 0.5 1 0.5 0] (apply-hann-window [1 1 1 1 1])))
    (is (almost-equal [0 (- 0.5) -1 (- 0.5) 0] (apply-hann-window [-1 -1 -1 -1 -1])))))

(deftest convert-rectangular-to-polar-test
  (testing "Basic tests"
    ; Make a template test to apply to multiple series of values
    (are [magnitudes phases real imaginary]
      (almost-equal
        {:magnitudes magnitudes :phases phases}
        (convert-rectangular-to-polar {:real real :imaginary imaginary}))
      [0 1] [0 0] [0 1] [0 0]
      [1] [Math/PI] [-1] [0])))
