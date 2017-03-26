(ns overpitch.pitch-shifting
  (:require [overpitch.time-scaling :refer [time-scale]]
            [overpitch.resampling :refer [resample]]))

(defn pitch-shift
  "Pitch-shifts the input data vector by the given scale"
  [input-data scale]
  (time-scale (resample input-data (/ 1 scale)) scale))

