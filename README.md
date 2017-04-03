# OverPitch
This library is an implementation of state-of-the-art pitch-shifting algorithms.

## Usage
Given a .wav file, you can pitch-shift it by a factor 1.2 by calling

```clj
(-main "your-audio-file.wav" "output.wav" 1.2)
```

The result will be written in `output.wav`.

## License

Distributed under the Eclipse Public License either version 1.0 or any later v
rsion.
