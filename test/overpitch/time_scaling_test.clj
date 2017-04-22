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

(deftest complex-conversion-test
  (testing "Basic tests"
    ; Make a template test to apply to multiple series of values
    (are [magnitudes phases real-parts imaginary-parts]
      ; Test both directions of conversion
      (and
        (almost-equal
          {:magnitudes magnitudes :phases phases}
          (convert-rectangular-to-polar real-parts imaginary-parts))
        (almost-equal
          {:real-parts real-parts :imaginary-parts imaginary-parts}
          (convert-polar-to-rectangular magnitudes phases)))
      [0 1] [0 0] [0 1] [0 0]
      [1] [Math/PI] [-1] [0]
      [1] [(- (/ Math/PI 4))] [(/ (Math/sqrt 2) 2)] [(/ (- (Math/sqrt 2)) 2)]
      ; All at once
      [0 1 1 1] [0 0 Math/PI (- (/ Math/PI 4))] [0 1 -1 (/ (Math/sqrt 2) 2)] [0 0  0 (/ (- (Math/sqrt 2)) 2)])))

(defn generate-cos
  ([k]
    (generate-cos k 1))
  ([k factor]
    (generate-cos k factor 0))
  ([k factor phase]
    (Math/cos (+ phase (/ (* factor 2 Math/PI k) frame-size)))))

(deftest fft-test
  (testing "All zeros"
    (is (almost-equal
      {:magnitudes (repeat frame-size 0)
       :phases     (repeat frame-size 0)}
      (fft (repeat frame-size 0)))))
  (testing "One harmonic"
    (is (almost-equal
      ; In principle, the magnitude should be 1. But since it is split between
      ; negative and positive frequencies, it should be 1/2. But it is finally
      ; length/2 because the fft does not scale the output.
      {:magnitudes (concat [0 (/ frame-size 2)] (repeat (- frame-size 2) 0))
       :phases     (repeat frame-size 0)}
      (fft (mapv #(generate-cos %) (range frame-size))))))
  (testing "Two harmonics"
    (is (almost-equal
      {:magnitudes (concat [0 (/ frame-size 2) (/ frame-size 2)] (repeat (- frame-size 3) 0))
       :phases     (repeat frame-size 0)}
      (fft (mapv #(+ (generate-cos %) (generate-cos % 2)) (range frame-size))))))
  (testing "One dephased harmonic"
    (is (almost-equal
      {:magnitudes (concat [0 (/ frame-size 2)] (repeat (- frame-size 2) 0))
       :phases     (concat [0 (/ Math/PI 5)] (repeat (- frame-size 2) 0))}
      (fft (mapv #(generate-cos % 1 (/ Math/PI 5)) (range frame-size)))))))
