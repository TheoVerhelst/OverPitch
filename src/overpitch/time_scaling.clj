(ns overpitch.time-scaling
  (:require [overpitch.utils :as utils]
            [clojure.math.numeric-tower :as math]
            [jtransforms.jtransforms :as jtransforms))

; Algorithm parameters
(def frame-size 1024)
(def synthesis-hopsize (/ frame-size 2))
(def sampling-rate 44100)
(def synthesis-hoptime (/ synthesis-hopsize sampling-rate))
(def time-frequencies (mapv #(/ (* %0  %1) %2) (range frame-size) sampling-rate frame-size))

(defn hann-window
  "The hann window function is defined as 0.5*(1 - cos(2*pi*x)), for x in [0, 1].
  It allows a smooth fading at the limits of a sound frame, and has nice
  mathematical properties.
  "
  [x]
  (cond
    (<= x 0) 0
    (>= x 1) 0
    :else (* 0.5 (- 1 (Math/cos (* 2 Math/PI x))))))

(defn apply-hann-window
  "Apply the hann window function to a frame."
  [frame]
  ; The dec makes hann-window output [0 0.5 1 0.5 0] rather than [0 0.34 0.9 0.9 0.34]
  (let [length (dec (count frame))]
    (vec
      (map-indexed
        ; the / maps [0, n] to [0, 1]
        (fn [i x] (* (hann-window (/ i length)) x))
        frame))))

(defn dft
  [frame]
  (let [result (double-array (* 2 (count frame)) frame)]
    (jtransforms/realForwardFull result)
    ; Now result contains real and imaginary values, split them
    (utils/split-channels result 2)))

(defn idft
  [frequencies phases]
  (let [result (double-array (utils/merge-channels frequencies phases))]
    (jtransforms/complexInverse result false)
    (first (utils/split-channels result 2))))

(defn map-phase
  [phase]
  (loop [phase phase]
    (cond
      (< phase -0.5) (recur (inc phase))
      (> phase 0.5)  (recur (dec phase))
      :else          phase)))

(defn transform-frame
  "Transforms the frame according to the phase vocoder, in order to time-scale it."
  [frame scale]
  (let [analysis-hopsize     (/ synthesis-hopsize scale)
        analysis-hoptime     (/ analysis-hopsize sampling-rate)
        frame                (apply-hann-window frame)
        [frequencies phases] (dft frame)]
    ; Call the inverse Fourrier transform with original frequencies
    (idft frequencies
      (loop [k 0 modified-phases [] instantaneous-frequencies []]
        (if (< k frame-size)
          (recur
            (inc k)
            (conj modified-phases
              (+ (last modified-phases)
              (* (last instantaneous-frequencies) synthesis-hoptime)))
            (conj instantaneous-frequencies
              (+ (time-frequencies k)
                (/ (map-phase
                  (-
                    (last modified-phases)
                    (last instantaneous-frequencies)
                    (* (time-frequencies k) analysis-hoptime))
                  analysis-hoptime))))
          modified-phases)))))


(defn time-scale
  [input-data scale]
  (let [length           (count input-data)
        analysis-hopsize (/ synthesis-hopsize scale)]
    ; Loop over the analysed buffer: i is the analysis index, j the synthesis index
    (loop [i 0 j 0 res []]
      (if (< i length)
        (recur (+ i analysis-hopsize) (+ j synthesis-hopsize)
          ; Add the transformed frame to the result at the synthesis index
          (utils/add-at-index res
            (transform-frame
              (subvec input-data i (min length (+ i frame-size)))
              scale)
            j))
        ; There is the returned value
        ; Ensure there is no number greater than 1 in res
        (map
          #(cond
            (> % 1)     1
            (< % (- 1)) (- 1)
            :else       %) res)))))
