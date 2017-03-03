# OverPitch
This library is just an implementation of state-of-the-art pitch-shifting
algorithms.

## Usage
Given a .wav file, you can pitch-shift it by calling

```clj
(overpitch-shift "your-audio-file.wav" "output.wav")
```

The result will be written in `output.wav`.

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or any later v
rsion.
