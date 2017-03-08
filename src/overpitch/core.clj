(ns overpitch.core
  (:require [overtone.live :as ov]
            [clojure.math.numeric-tower :as math]
            [clojure.java.io :as io]
  )
)

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
    (mapv + v1* v2*)
  )
)

(defn hann-window
  "The hann window function is defined as 0.5*(1 - cos(2*pi*x)), for x in [0, 1].
  It allows a smooth fading at the limits of a sound frame, and has nice
  mathematical properties.
  "
  [x]
  (cond
    (<= x 0) 0
    (>= x 1) 0
    :else (* 0.5 (- 1 (Math/cos (* 2 Math/PI x))))
  )
)

(defn apply-hann-window
  "Apply the hann window function to a frame."
  [frame]
  ; The dec makes hann-window output [0 0.5 1 0.5 0] rather than [0 0.34 0.9 0.9 0.34]
  (let [length (dec (count frame))]
    (vec
      (map-indexed
        ; the / function maps [0, n] to [0, 1]
        (fn [i x] (* (hann-window (/ i length)) x))
        frame
      )
    )
  )
)

(defn transform-frame
  "Transforms the frame according to the phase vocoder, in order to time-scale it."
  [frame scale]
  (let [frame (apply-hann-window frame)]
    frame
  )
)

(defn resample
  "Resample the input frame to the given scale. For exemple, if the scale
  parameter is 2, then the output will have two times more samples (the lacking
  samples will be calculated). If the sound is then played at the original
  sampling rate, it will sound lower-pitched (exactly one octave lower).
  "
  [input-data scale]
  (let [in-length  (count input-data)
        out-length (int (* in-length scale))
        input-step (/ 1 (dec in-length))]
    (vec
      (for [i (range out-length)]
        ; the t variables and input-step are numbers in [0, 1] that represent
        ; absolutely the position in the buffer, where 1 is the end of the
        ; buffer. This allows to compare input and output positions easily.
        (let [t           (/ i out-length)
              prev-sample (math/floor (/ i scale))
              next-sample (math/ceil (/ i scale))
              prev-t      (/ prev-sample in-length)
              next-t      (/ next-sample in-length)
              prev-value  (input-data prev-sample)
              next-value  (input-data next-sample)]
          (+ prev-value (* (- next-value prev-value) (/ (- t prev-t) input-step)))
        )
      )
    )
  )
)

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
          (add-at-index res
            (transform-frame
              (subvec input-data i (min length (+ i frame-size)))
              scale
            )
            j
          )
        )
        ; There is the returned value
        ; Ensure there is no number greater than 1 in res
        (map
          #(cond
            (> % 1)     1
            (< % (- 1)) (- 1)
            :else       %
          ) res
        )
      )
    )
  )
)

(defn merge-channels
  [channels-data]
  (reduce into (apply mapv vector channels-data))
)

(defn split-channels
  [input-data n-channels]
  (for [channel (range n-channels)]
    (vec
      (for [[i x] (map-indexed vector input-data)
            :when (= (mod i n-channels) channel)]
        x
      )
    )
  )
)

(defn pitch-shift
  "Pitch-shifts the input data vector by the given scale"
  [input-data n-channels scale]
  (let [channels-data (split-channels input-data n-channels)]
    (merge-channels
      (for [channel channels-data]
        (time-scale (resample channel scale) scale)
      )
    )
  )
)

(defn overpitch-shift
  "Shifts the pitch of a wav file, and writes the result to the given path."
  [input-path output-path scale]
  ; If scale is 1, just copy the file to the output-path
  (if (= scale 1)
    (io/copy (io/file input-path) (io/file output-path))
    ; else
    (let [input-buffer      (ov/load-sample input-path)
          input-buffer-info (ov/buffer-info input-buffer)
          n-channels        (:n-channels input-buffer-info)
          output-buffer     (ov/buffer (:size input-buffer-info) n-channels)
          pitched-data      (pitch-shift (vec (ov/buffer-data input-buffer)) n-channels scale)]
      (println pitched-data)
      (ov/write-wav pitched-data output-path (:rate input-buffer-info) n-channels)
    )
  )
)

(defn -main
  "Main function"
  [& args]
  (overpitch-shift "test.wav" "out.wav" 2)
)
