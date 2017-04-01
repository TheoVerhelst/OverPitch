(ns overpitch.utils
  (:require [clojure.math.numeric-tower :as math]))

(defn add-at-index
  "Merges two vectors at position pos in the first vector. Overlapping elements
  are added.

  Exemple:
    (add-at-index [0 1 2 3 4] [10 10 10] 3) returns [0 1 2 13 14 10]
  "
  [v1 v2 pos]
  (when (< pos 0) (throw (Exception. "pos must be greater than 0")))
  (let [result-length (max (count v1) (+ (count v2) pos))
        ; v1* is v1 filled with zeros so that its length reaches result-length
        v1* (into v1 (repeat (max 0 (- result-length (count v1))) 0))
        v2* (into (vec (repeat (max 0 (- result-length (count v2))) 0)) v2)]
    (mapv + v1* v2*)))


(defn merge-channels
  [channels-data]
  (reduce into (apply mapv vector channels-data)))

(defn split-channels
  [input-data n-channels]
  (for [channel (range n-channels)]
    (vec
      (for [[i x] (map-indexed vector input-data)
            :when (= (mod i n-channels) channel)]
        x))))

(defn almost-equal
  "Checks whether the difference between x and y is less than epsilon. If x and
  y are sequences, then compare each element, and return true is all elements
  are almost equals respectively to the other sequence."
  [x y]
  (let [epsilon 0.0000001
        compare_ (fn [a b] (< (math/abs (- a b)) epsilon))]
    (if (sequential? x)
      (every? true? (map compare_ x y))
      (compare_ x y))))
