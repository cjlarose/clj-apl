(defproject apl "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [net.mikera/core.matrix "0.35.0"]]
  :plugins [[lein-cloverage "1.0.6"]]
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.9.0"]]}})
