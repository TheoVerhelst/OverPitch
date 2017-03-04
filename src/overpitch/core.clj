(ns overpitch.core
  (:require [overtone.live :as ov])
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
  [x]
  (cond
    (<= x 0) 0
    (>= x 1) 0
    :else (* 0.5 (- 1 (Math/cos (* 2 Math/PI x))))
  )
)

(defn transform-frame
  [frame]
  (let [length (count frame)]
    (mapv #(* (hann-window (/ (% 0) (dec length))) (% 1)) (map-indexed vector frame))
  )
)

(defn resample
  [input-data n-channels scale]
  input-data
)

(defn time-scale
  [input-data n-channels scale]
  (let [length (/ (count input-data) n-channels)
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
              (subvec input-data
                (* i n-channels)
                (* (min length (+ i frame-size)) n-channels)
              )
            )
            (* j n-channels)
          )
        )
        ; Ensure there is no number greater than 1
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

(defn pitch-shift
  "Pitch-shifts the input data vector by the given scale"
  [input-data n-channels scale]
  (pitch-shift (resample input-data n-channels scale))
)

(defn overpitch-shift
  "Shifts the pitch of a wav file, and writes the result to the given path."
  [input-path output-path scale]
  (let [input-buffer      (ov/load-sample input-path)
        input-buffer-info (ov/buffer-info input-buffer)
        n-channels        (:n-channels input-buffer-info)
        output-buffer     (ov/buffer (:size input-buffer-info) n-channels)
        pitched-data      (pitch-shift (vec (ov/buffer-data input-buffer)) n-channels scale)]
    (println (filter #(> % 1) pitched-data))
    (ov/write-wav pitched-data output-path (:rate input-buffer-info) n-channels)
  )
)

(defn -main
  "Main function"
  [& args]
  (overpitch-shift "test.wav" "out.wav" 1)
)
