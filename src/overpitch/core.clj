(ns overpitch.core
  (:require [overtone.live :as overtone]
            [clojure.java.io :as io]
            [overpitch.resampling :refer [resample]]
            [overpitch.time-scaling :refer [time-scale]]
            [overpitch.pitch-shifting :refer [pitch-shift]]
            [overpitch.utils :refer [split-channels merge-channels clip]])
  (:gen-class))

(defn transform-wav
  "Transform the content of a wav file, and writes the result to the given path."
  [input-path output-path transformation scale]
  ; If scale is 1, just copy the file to the output-path
  (if (= scale 1)
    (io/copy (io/file input-path) (io/file output-path))
    ; else
    (let [input-buffer         (overtone/load-sample input-path)
          input-buffer-info    (overtone/buffer-info input-buffer)
          n-channels           (:n-channels input-buffer-info)
          input-data           (vec (overtone/buffer-data input-buffer))]
      (overtone/write-wav
        (clip
          (merge-channels
            (for [channel (split-channels input-data n-channels)]
              (transformation channel scale))))
        output-path (:rate input-buffer-info) n-channels))))

(defn -main
  "Main function"
  [& args]
  (let [args (vec args)
        input-file (args 0)
        output-file (args 1)
        scale (double (bigdec (args 2)))]
    ;(transform-wav input-file output-file pitch-shift scale)
    (println "Finished, you can hit Ctrl+C and hear the result in " output-file)))
