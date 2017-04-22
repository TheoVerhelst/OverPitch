(defproject overpitch "0.1.0-SNAPSHOT"
  :description "Pitch-scaling library for musical audio based on OverTone."
  :dependencies [
    [org.clojure/clojure "1.8.0"]
    [overtone "0.10.1"]
    [org.clojure/math.numeric-tower "0.0.4"]
    [net.mikera/core.matrix "0.57.0"]
    [net.sourceforge.jtransforms/jtransforms "2.4.0"]]
  :main overpitch.core
  :jvm-opts ["-Dcom.sun.management.jmxremote"
             "-Dcom.sun.management.jmxremote.ssl=false"
             "-Dcom.sun.management.jmxremote.authenticate=false"
             "-Dcom.sun.management.jmxremote.port=43210"])
