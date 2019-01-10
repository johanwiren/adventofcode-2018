(defproject aoc "0.1.0-SNAPSHOT"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/spec.alpha "0.2.176"]
                 [org.clojure/test.check "0.10.0-alpha3"]

                 [clojure-lanterna "0.9.7"]
                 [com.clojure-goes-fast/clj-memory-meter "0.1.2"]
                 [com.google.guava/guava "27.0.1-jre"]
                 [metasoarous/oz "1.4.1"]
                 [weavejester/dependency "0.2.1"]]
  :profiles {}
  :jvm-opts ["-Djdk.attach.allowAttachSelf"])
