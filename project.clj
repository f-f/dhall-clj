(defproject dhall-clojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0-alpha4"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/tools.reader "1.0.3"]
                 [org.clojure/core.match "0.3.0-alpha5"]]
                 ;;[instaparse "1.4.8"]])
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.10.0-alpha2"]]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.22.0"]]}
             :uberjar {:aot :all}}
  :test-paths ["test" "src"]
  :test-refresh {:quiet true
                 :changes-only true})
