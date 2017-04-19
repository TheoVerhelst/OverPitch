(ns overpitch.resampling
  (:require [overpitch.utils :refer [clip]]
            [clojure.math.numeric-tower :as math]
            [clojure.core.matrix :as matrix]))

(defn linear-interpolation
  "Evaluates the linear interpolation between y1 and y2, at the time t.
  This function also uses y0 and y3, so that it has the same interface as a
  cubic-spline-interpolation."
  [[y0 y1 y2 y3] t]
  (+ y1 (* t (- y2 y1))))

(defn cubic-spline-interpolation
  "Evaluate the cubic spline interpolation between y1 and y2 at the time t,
  by taking into account previous and next values y0 and y3."
  [[y0 y1 y2 y3] t]
  (let [interpolation-matrix [[1 0 0 0][0 0 0.5 0][-3 3 -1 -0.5][2 -2 0.5 0.5]]
        factors (matrix/mmul interpolation-matrix [y1 y2 (- y2 y0) (- y3 y1)])]
    ; inner product between the factors and the 4 first powers of t
    ; i.e. the formula of a cubic spline, with our previously calculated factors
    (matrix/inner-product factors (mapv math/expt (repeat t) (range 4)))))

(defn resample
  "Resamples the input frame to the given scale. For example, if the scale
  parameter is 2, then the output will have two times more samples (the lacking
  samples will be calculated). If the sound is then played at the original
  sampling rate, it will sound lower-pitched (exactly one octave lower).

  We need to infer the values of the before-the-first and past-the-last samples,
  so we decided that input-data[-1] will be set to input-data[0], and
  input-data[n] to input-data[n - 1], with n being the length of input-data."
  [input-data scale]
  (let [in-length  (count input-data)
        out-length (math/floor (* in-length scale))]
    ; Cubic interpolation may lead to values out of [-1; 1], so we have to clip
    ; the output the hard way
    (clip (vec (cons (first input-data)
            ; i is the output index
      (for [i      (range 1 out-length)
            ; j is i scaled in order to compare it to input indices
      :let [j      (/ i scale)
            j1     (int j)
            j2     (min (dec in-length) (inc j1))
            j0     (max 0 (dec j1))
            j3     (min (dec in-length) (inc j2))
            ; t is a value between 0 and 1, that matches the position
            ; of the output sample relatively to the surrounding input samples
            t      (- j j1)
            ; values contains the values of the input samples surrounding the
            ; output sample.
            values (mapv input-data [j0 j1 j2 j3])]]
        (cond
          (= t 0) (values 1)
          (= t 1) (values 2)
          :else (cubic-spline-interpolation values t))))))))
