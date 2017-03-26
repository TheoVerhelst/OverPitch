(ns overpitch.time-scaling
  (:require [overpitch.utils :as utils]))

(defn hann-window
  "The hann window function is defined as 0.5*(1 - cos(2*pi*x)), for x in [0, 1].
  It allows a smooth fading at the limits of a sound frame, and has nice
  mathematical properties.
  "
  [x]
  (cond
    (<= x 0) 0
    (>= x 1) 0
    :else (* 0.5 (- 1 (Math/cos (* 2 Math/PI x)))))) ; TODO Try to use clojure.math.numeric-tower rather than Math

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

(defn transform-frame
  "Transforms the frame according to the phase vocoder, in order to time-scale it."
  [frame scale]
  (let [frame (apply-hann-window frame)]
    frame))


(defn time-scale
  [input-data scale]
  (let [length (count input-data)
        ; Algorithm parameters
        frame-size 1024
        synthesis-hopsize (/ frame-size 2)
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
