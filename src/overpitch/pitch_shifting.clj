(ns overpitch.pitch-shifting
  (:require [overpitch.time-scaling :refer [time-scale]]
            [overpitch.resampling :refer [resample]]))

(defn pitch-shift
  "Pitch-shifts the input data vector by the given scale"
  [input-data scale]
  (resample (time-scale input-data scale) (/ 1 scale)))

