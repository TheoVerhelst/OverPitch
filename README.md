# OverPitch
This library is an implementation of state-of-the-art pitch-shifting algorithms.

## Requirements
This project is written in Clojure, and the dependencies are listed in
`project.clj`. The simplest way to use this project is to install
[Leiningen](https://leiningen.org/) and run the command `lein deps`.
**Note:** you may need to start a JACK server in order to properly start
Overtone.

## Usage
Given a the file `your-audio-file.wav` file, you can pitch-shift it by a factor
1.2 by calling either:

```clj
(-main "your-audio-file.wav" "output.wav" 1.2)
```

from inside the project repl (by running `lein repl`), or by running

```bash
lein run "your-audio-file.wav" "output.wav" 1.2
```

from the command line.

The result will be written in `output.wav`.

## Documentation
An in-depth theoretical explanation of the process involved in the
pitch-shifting algorithm implemented by this library is available in the folder
`docs/`.

## License
Distributed under the Eclipse Public License either version 1.0 or any later
version.
