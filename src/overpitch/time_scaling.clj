(ns overpitch.time-scaling
  (:require [overpitch.utils :as utils]
            [clojure.math.numeric-tower :as math])
  (:import  (edu.emory.mathcs.jtransforms.fft DoubleFFT_1D)))

; Algorithm parameters
(def frame-size 1024)
(def synthesis-hopsize (/ frame-size 4))
(def sampling-rate 44100) ; TODO parametrize this
(def synthesis-hoptime (/ synthesis-hopsize sampling-rate))
(def time-frequencies (mapv #(/ (* % sampling-rate) frame-size) (range frame-size)))

; Instantiates a FFT object from jtransforms
(def jtransforms-fft-instance (new DoubleFFT_1D frame-size))

(defn hann-window
  "The hann window function is defined as 0.5*(1 - cos(2*pi*x)), for x in [0, 1].
  It allows for a smooth fading at the limits of a sound frame, and has nice
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

(defn rectangular-to-polar
  "Converts a map of numbers from real-imaginary coordinates to polar
  coordinates. The numbers argument must be a map containing a vector or values
  labelled :real, and another vector of values labelled :imaginary."
  [real-parts imaginary-parts]
  [(mapv #(Math/hypot %1 %2) real-parts imaginary-parts)
   ; Watch out, atan2 takes y (the imaginary part) as first argument
   (mapv #(Math/atan2 %2 %1) real-parts imaginary-parts)])


(defn polar-to-rectangular
  "Converts a map of numbers from real-imaginary coordinates to polar
  coordinates."
  [magnitudes phases]
  [(mapv #(* %1 (Math/cos %2)) magnitudes phases)
   (mapv #(* %1 (Math/sin %2)) magnitudes phases)])

(defn phases-trigo-to-unit
  [phases]
    (vec (for [phase phases :let [scaled (/ phase 2 Math/PI)]]
      ; scaled is in the range [-0.5, 0.5], map it to [0, 1]
      (if (< scaled 0) (inc' scaled) scaled))))

(defn phases-unit-to-trigo
  [phases]
    (vec (for [phase phases :let [mapped (if (> phase 0.5) (dec' phase) phase)]]
      (* mapped 2 Math/PI))))

(defn filter-zeros
  "Replaces all numbers almost equal to zero by zero."
  [numbers]
  (mapv #(if (utils/almost-equal 0 %) 0 %) numbers))

(defn fft
  [frame]
  (let [result (double-array (* 2 frame-size) frame)]
    ; call realForwardFull method on the jtransforms fft object, with result as
    ; argument. The array result will be overwritten by the method
    (.realForward jtransforms-fft-instance result)
    ; JTransforms puts Re[n/2] in result[1], we don't need that
    (aset-double result 1 0)
    ; Now result contains real and imaginary values, convert it to polar
    ; coordinates in a map, by first filtering almost-zeros values. If we don't
    ; filter these values, the phases will be garbaged. Then, convert the pases
    ; to the [0, 1] interval, as needed by the phase-vocoder algorithm.
    (let [[magnitudes phases] (apply rectangular-to-polar (utils/split-channels (filter-zeros result) 2))]
      [magnitudes (phases-trigo-to-unit phases)])))

(defn ifft
  [magnitudes phases]
  (let [rectangular-bins (polar-to-rectangular magnitudes (phases-unit-to-trigo phases))
        result (double-array (utils/merge-channels rectangular-bins))]
    (aset-double result 1 0)
    (.realInverse jtransforms-fft-instance result true)
    (vec result)))

(defn map-phase
  [phase]
  (- phase (math/ceil (- phase 0.5))))

(defn instaneous-frequencies
  ([phases prev-phases analysis-hoptime]
  "Overload taking time-frequencies as rough frequencies estimates"
  (instaneous-frequencies time-frequencies phases prev-phases analysis-hoptime))

  ([frequencies phases prev-phases analysis-hoptime]
  "Overload taking with custom rough frequencies estimates, for unit tests."
  (mapv
    (fn [frequency phase prev-phase]
      ; The next instantaneous frequency is the value of the k-th frequency
      ; plus a small offset
      (+ frequency (/ (map-phase
        (- phase prev-phase (* frequency analysis-hoptime))) analysis-hoptime)))
    frequencies
    phases
    prev-phases)))

(defn propagate-phases
  [inst-frequencies mod-phases]
    (mapv (fn [frequency phase] (+ phase (* frequency synthesis-hoptime))) inst-frequencies mod-phases))

(defn split-in-frames
  [input-data analysis-hopsize]
  (let [length (count input-data)]
    (for [i     (range 0 length analysis-hopsize)
    :let [slice (subvec input-data i (min length (+ i frame-size)))]]
      (concat slice (repeat 0 (- frame-size (count slice)))))))

(defn overlap-and-add-frames
  [frames]
  (loop [m 0 res []]
    (if (< m (count frames))
      (recur (inc m) (utils/add-at-index res (frames m) (* m synthesis-hopsize)))
      res)))

(defn time-scale
  [input-data scale]
  (let [length           (count input-data)
        analysis-hopsize (/ synthesis-hopsize scale)
        analysis-hoptime (/ analysis-hopsize sampling-rate)
        frames           (mapv apply-hann-window (split-in-frames input-data analysis-hopsize))
        spectrums        (mapv fft frames)]
    (overlap-and-add-frames
      (loop [m                     0
             prev-mod-phases       []
             modified-frames       []]
        (if (< m (count frames))
          (let [[magnitudes phases] (spectrums m)
                mod-phases
                  (if (= m 0)
                    ; If there is no previous frame, use the normal phase as modified phase
                    phases
                    ; If there is a previous frame, propagate the phases from the previous ones
                    (let [[_ prev-phases]     (spectrums (dec m))
                          ; If there is a previous frame, compute the instantaneous frequencies
                          inst-frequencies    (instaneous-frequencies phases prev-phases analysis-hoptime)]
                      (propagate-phases inst-frequencies prev-mod-phases)))]
                (recur (inc m) mod-phases (conj modified-frames (apply-hann-window (ifft magnitudes mod-phases))))))
        modified-frames))))
