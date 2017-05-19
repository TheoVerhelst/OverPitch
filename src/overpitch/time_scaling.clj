(ns overpitch.time-scaling
  (:require [overpitch.utils :as utils]
            [clojure.math.numeric-tower :as math])
  (:import  (edu.emory.mathcs.jtransforms.fft DoubleFFT_1D)))

; Algorithm parameters
(def frame-size 1024)
(def synthesis-hopsize (/ frame-size 2))
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
    (if (<= length 0)
      [(first frame)]
      (vec
        (map-indexed
          ; the / maps [0, n] to [0, 1]
          (fn [i x] (* (hann-window (/ i length)) x))
          frame)))))

(defn rectangular-to-polar
  "Converts a map of numbers from real-imaginary coordinates to polar
  coordinates. The numbers argument must be a map containing a vector or values
  labelled :real, and another vector of values labelled :imaginary."
  [real-parts imaginary-parts]
  [(mapv #(Math/hypot %1 %2) real-parts imaginary-parts)
   ; Watch out, atan2 takes y (the imaginary part) as first argument
   (mapv #(Math/atan2 %2 %1) real-parts imaginary-parts)])

(defn map-phase
  [phase]
    (- phase (Math/round phase)))

(defn polar-to-rectangular
  "Converts a map of numbers from real-imaginary coordinates to polar
  coordinates."
  [magnitudes phases]
  [(mapv #(* %1 (Math/cos %2)) magnitudes phases)
   (mapv #(* %1 (Math/sin %2)) magnitudes phases)])

(defn phases-trigo-to-unit
  [phases]
  (mapv (fn [phase] (let [scaled (/ phase 2 Math/PI)]
      ; scaled is in the range [-0.5, 0.5], map it to [0, 1]
      (if (< scaled 0) (inc' scaled) scaled)))
    phases))

(defn phases-unit-to-trigo
  [phases]
  (mapv (fn [phase] (let [mapped (if (> phase 0.5) (dec' phase) phase)]
      (* mapped 2 Math/PI)))
    phases))

(defn filter-zeros
  "Replaces all numbers almost equal to zero by zero."
  [numbers]
  (mapv #(if (utils/almost-equal 0 %) 0 %) numbers))

(defn fft
  [frame]
  (let [result (double-array (* 2 frame-size) frame)]
    ; call realForwardFull method on the jtransforms fft object, with result as
    ; argument. The array result will be overwritten by the method
    (.realForwardFull jtransforms-fft-instance result)
    ; JTransforms puts Re[n/2] in result[1], we don't need that
    (aset-double result 1 0)
    ; Now result contains real and imaginary values, convert it to polar
    ; coordinates in a map, by first filtering almost-zeros values. If we don't
    ; filter these values, the phases will be garbaged. Then, convert the pases
    ; to the [0, 1] interval, as needed by the phase-vocoder algorithm.
    (let [[magnitudes phases] (apply rectangular-to-polar (utils/split-channels (filter-zeros result) 2))]
      [magnitudes (phases-trigo-to-unit phases)])))

(defn ifft
  [[magnitudes phases]]
  (let [rectangular-bins (polar-to-rectangular magnitudes (phases-unit-to-trigo phases))
        result (double-array (utils/merge-channels rectangular-bins))]
    (.complexInverse jtransforms-fft-instance result true)
    (first (utils/split-channels (vec result) 2))))

(defn split-in-frames
  [input-data analysis-hopsize]
  (let [length (count input-data)]
    (mapv
      (fn [i]
        (let [slice (subvec input-data i (min length (+ i frame-size)))]
          (concat slice (repeat 0 (- frame-size (count slice))))))
      (range 0 length analysis-hopsize))))

(defn overlap-and-add-frames
  [frames]
  (let [frames (mapv apply-hann-window frames)
        length (+ (* (count frames) synthesis-hopsize) (- frame-size synthesis-hopsize))]
    (for [frame (subvec frames 40 48)] (print frame))
    (mapv
      (fn [i]
        (reduce +
          (map-indexed
            (fn [m frame]
              (let [in-frame-index (- i (* m synthesis-hopsize))]
                (if (<= 0 in-frame-index (dec frame-size))
                  (frame in-frame-index)
                  0)))
            frames)))
      (range length))))

(defn instaneous-frequencies
  [spectrums analysis-hoptime]
  (let [phase-increments (mapv #(* analysis-hoptime %) time-frequencies)]
    (conj
      (mapv (fn [m] (let [[_1 phases]      (spectrums m)
                          [_2 next-phases] (spectrums (inc m))]
        (mapv (fn [k] (let [phase-inc            (phase-increments k)
                            predicted-next-phase (+ phase-inc (phases k))
                            phase-error          (map-phase (- (next-phases k) predicted-next-phase))]
            (+ (time-frequencies k) (/ phase-error analysis-hoptime))))
          (range frame-size))))
        (range (dec (count spectrums))))
      time-frequencies)))

(defn adjust-phases
  [instantaneous-frequencies spectrums synthesis-hoptime]
    (loop [m 1 modified-spectrums [(first spectrums)]]
      (if (< m (count instantaneous-frequencies))
        ; prev-inst-frequencies is the vector of inst frequencies for each
        ; frequency channel of the previous frame
        (let [[magnitudes phases]   (spectrums m)
              prev-inst-frequencies (instantaneous-frequencies (dec m))
              [_ prev-mod-phases]   (modified-spectrums (dec m))]
          (recur (inc m) (conj modified-spectrums [magnitudes
            (mapv
              (fn [prev-mod-phase prev-inst-frequency]
                (+ prev-mod-phase (* prev-inst-frequency synthesis-hoptime)))
              prev-mod-phases prev-inst-frequencies)])))
        modified-spectrums)))

(defn time-scale
  [input-data scale]
  (let [length             (count input-data)
        analysis-hopsize   (/ synthesis-hopsize scale)
        analysis-hoptime   (/ analysis-hopsize sampling-rate)
        frames             (mapv apply-hann-window (split-in-frames input-data analysis-hopsize))
        spectrums          (mapv fft frames)
        inst-frequencies   (instaneous-frequencies spectrums analysis-hoptime)
        modified-spectrums (adjust-phases inst-frequencies spectrums synthesis-hoptime)]
    (overlap-and-add-frames (mapv ifft modified-spectrums))))
