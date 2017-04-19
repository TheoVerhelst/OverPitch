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
    (vec
      (map-indexed
        ; the / maps [0, n] to [0, 1]
        (fn [i x] (* (hann-window (/ i length)) x))
        frame))))

(defn convert-rectangular-to-polar
  "Converts a map of numbers from real-imaginary coordinates to polar
  coordinates. The numbers argument must be a map containing a vector or values
  labelled :real, and another vector of values labelled :imaginary."
  [numbers]
  (let [real-parts (:real numbers) imaginary-parts (:imaginary number)
        magnitudes (mapv #(Math/hypot (? 0) (? 1)) real-parts imaginary-parts)
        ; Watch out, atan2 takes y (the imaginary part) as first argument
        phases (mapv #(Math/atan2 (? 1) (? 0)) real-parts imaginary-parts))
    {:magnitudes magnitudes :phases phases})

(defn fft
  [frame]
  (let [result (double-array (* 2 frame-size) frame)]
    ; call realForwardFull method on the jtransforms fft object, with result as
    ; argument. The array result will be overwritten by the method
    (.realForwardFull jtransforms-fft-instance result)
    ; Now result contains real and imaginary values, put it in a map and convert
    ; it to polar coordinates
    (convert-rectangular-to-polar
      (zipmap [:real :imaginary] (utils/split-channels result 2)))))

(defn ifft
  [magnitudes phases]
  (let [result (double-array (utils/merge-channels [magnitudes phases]))]
    (.complexInverse jtransforms-fft-instance result false)
    (first (utils/split-channels result 2))))

(defn map-phase
  [phase]
  (loop [phase phase]
    (cond
      (< phase -0.5) (recur (inc phase))
      (> phase 0.5)  (recur (dec phase))
      :else          phase)))

(defn phase-vocoder
  [phases next-phases analysis-hoptime]
  (mapv
    ; The next instantaneous frequency is the value of the k-th frequency
    ; plus a small offset
    #(+ (time-frequencies %)
      (/
        (map-phase
          (- (next-phases %) (phases %) (* (time-frequencies %) analysis-hoptime)))
        analysis-hoptime))
    (range frame-size)))

(defn propagate-phase
  [frequencies phases]
    (mapv #(+ (phases %) (* (frequencies %) synthesis-hoptime)) (range frame-size)))

(defn split-in-frames
  [input-data analysis-hopsize]
  (let [length (count input-data)]
    (for [i     (range 0 length analysis-hopsize)
    :let [slice (subvec input-data i (min length (+ i frame-size)))]]
      (apply-hann-window (into slice (repeat 0 (- frame-size length)))))))

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
             prev-inst-frequencies []
             prev-mod-phases       []
             modified-frames       []]
        (if (< m (count frames))
          (let [magnitudes       (:magnitudes (spectrums m))
                phases           (:phases (spectrums m))
                next-phases      (if (< (inc m) (count frames)) (:phases (spectrums (inc m))) nil)
                inst-frequencies (if (< (inc m) (count frames)) (phase-vocoder phases next-phases analysis-hoptime) nil)
                mod-phases       (if (> 0 m)
                                   (propagate-phase prev-inst-frequencies prev-mod-phases)
                                   ; If we are at m = 0, then the modified phase
                                   ; is set to the original phase
                                   phases)]
            (recur (inc m) inst-frequencies mod-phases
              (conj modified-frames (apply-hann-window (ifft magnitudes mod-phases)))))
        modified-frames)))))
