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

(defn fft
  [frame]
  (let [result (double-array (* 2 frame-size) frame)]
    ; call realForwardFull method on the jtransforms fft object, with result as
    ; argument. The array result will be overwritten by the method
    (.realForwardFull jtransforms-fft-instance result)
    ; Now result contains real and imaginary values, return them in a map
    (zipmap [:magnitudes :phases] (utils/split-channels result 2))))

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
  [k phase next-phase analysis-hoptime]
  ; The next instantaneous frequency is the value of the k-th frequency
  ; plus a small offset
  (+ (time-frequencies k)
    (/
      (map-phase
        (- next-phase phase (* (time-frequencies k) analysis-hoptime)))
      analysis-hoptime)))

(defn propagate-phase
  [phase frequency]
    (+ phase (* frequency synthesis-hoptime)))

(defn split-in-frames
  [input-data analysis-hopsize]
  (let [length (count input-data)]
    (for [i (range 0 length analysis-hopsize)]
      (apply-hann-window (subvec input-data i (min length (+ i frame-size)))))))

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
      (loop [m                                  0
             previous-instantaneous-frequencies []
             previous-modified-phases           []
             modified-frames                    []]
        (if (< m (count frames))
          (let [magnitudes                (:magnitudes (spectrums m))
                phases                    (:phases (spectrums m))
                next-phases               (:phases (spectrums (inc m)))
                instantaneous-frequencies (mapv
                                            #(phase-vocoder %
                                              (phases %)
                                              (next-phases %)
                                              analysis-hoptime)
                                            (range frame-size))
                modified-phases           (if (> 0 m)
                                            (mapv
                                              #(propagate-phase
                                                (previous-instantaneous-frequencies %)
                                                (previous-modified-phases %))
                                              (range frame-size))
                                            ; If we are at m = 0, then the modified phase
                                            ; is set to the original phase
                                            phases)]
            (recur (inc m) instantaneous-frequencies modified-phases
              (conj modified-frames (ifft magnitudes modified-phases))))
        modified-frames)))))
