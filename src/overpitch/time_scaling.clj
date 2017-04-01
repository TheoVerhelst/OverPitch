(ns overpitch.time-scaling
  (:require [overpitch.utils :as utils]
            [clojure.math.numeric-tower :as math])
  (:import  (edu.emory.mathcs.jtransforms.fft DoubleFFT_1D)))

; Algorithm parameters
(def frame-size 1024)
(def synthesis-hopsize (/ frame-size 2))
(def sampling-rate 44100)
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
  (let [result (double-array (* 2 (count frame)) frame)]
    ; call realForwardFull method on the jtransforms fft object, with result as
    ; argument. The array result will be overwritten by the method
    (.realForwardFull jtransforms-fft-instance result)
    ; Now result contains real and imaginary values, split them
    (utils/split-channels result 2)))

(defn ifft
  [magnitudes phases]
  (let [result (double-array (utils/merge-channels magnitudes phases))]
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

(defn transform-frame
  "Transforms the frame according to the phase vocoder, in order to time-scale it."
  [frame scale]
  (let [analysis-hopsize    (/ synthesis-hopsize scale)
        analysis-hoptime    (/ analysis-hopsize sampling-rate)
        frame               (apply-hann-window frame)
        [magnitudes phases] (fft frame)]
    ; Call the inverse Fourrier transform with original frequencies
    (ifft magnitudes
      ; Loop over all frequencies in order to construct the array of modified
      ; phases (according to the phase propagation technique in phase vocoder-TSM)
      (loop [k 0 modified-phases [] instantaneous-frequencies []]
        (if (< k frame-size)
          (recur
            (inc k)
            ; The next modified phase is the sum of the previous one and the
            ; real phase advance
            (conj modified-phases
              (+ (last modified-phases)
                 (* (last instantaneous-frequencies) synthesis-hoptime)))
            (conj instantaneous-frequencies
              (phase-vocoder k (phases k) (phases (inc k)) analysis-hoptime)))
          modified-phases)))))


(defn time-scale
  [input-data scale]
  (let [length           (count input-data)
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
